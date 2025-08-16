#!/usr/bin/env -S deno run --allow-read --allow-write

/**
 * org-parinfer.ts
 * 
 * Org-modeファイル内のEmacs Lispコードブロックに対してParinferを適用するCLIツール
 * 
 * 使用方法:
 *   deno run --allow-read --allow-write org-parinfer.ts [options] <file.org>
 * 
 * オプション:
 *   --mode <mode>  使用するParinferモード (indent, paren, smart) デフォルト: smart
 *   --in-place     ファイルを直接編集する
 *   --dry-run      実際の変更を行わず、結果を表示するのみ
 *   --help         ヘルプを表示
 */

import { parse } from "https://deno.land/std@0.208.0/flags/mod.ts";
import parinfer from "npm:parinfer@3.13.1";

interface CodeBlock {
  startLine: number;
  endLine: number;
  content: string;
  language: string;
}

class OrgModeProcessor {
  /**
   * Org-modeファイルからEmacs Lispコードブロックを抽出
   */
  extractEmacsLispBlocks(content: string): CodeBlock[] {
    const lines = content.split("\n");
    const blocks: CodeBlock[] = [];
    let inBlock = false;
    let currentBlock: CodeBlock | null = null;
    let blockContent: string[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      
      // コードブロックの開始を検出
      if (line.match(/^#\+begin_src\s+(emacs-lisp|elisp)/i)) {
        inBlock = true;
        currentBlock = {
          startLine: i,
          endLine: -1,
          content: "",
          language: line.match(/^#\+begin_src\s+(emacs-lisp|elisp)/i)?.[1] || "emacs-lisp"
        };
        blockContent = [];
      }
      // コードブロックの終了を検出
      else if (inBlock && line.match(/^#\+end_src/i)) {
        if (currentBlock) {
          currentBlock.endLine = i;
          currentBlock.content = blockContent.join("\n");
          blocks.push(currentBlock);
        }
        inBlock = false;
        currentBlock = null;
        blockContent = [];
      }
      // コードブロック内のコンテンツを収集
      else if (inBlock) {
        blockContent.push(line);
      }
    }

    return blocks;
  }

  /**
   * Parinferを適用してコードを修正
   */
  applyParinfer(code: string, mode: "indent" | "paren" | "smart" = "smart") {
    const options = {
      // Emacs Lisp用の設定
      commentChars: [";"],
      openParenChars: ["(", "["],
      closeParenChars: [")", "]"]
    };

    switch (mode) {
      case "indent":
        return parinfer.indentMode(code, options);
      case "paren":
        return parinfer.parenMode(code, options);
      case "smart":
      default:
        return parinfer.smartMode(code, options);
    }
  }

  /**
   * Org-modeファイルを処理
   */
  processOrgFile(content: string, mode: "indent" | "paren" | "smart" = "smart"): {
    content: string;
    changes: number;
    errors: Array<{ block: number; error: any }>;
  } {
    const lines = content.split("\n");
    const blocks = this.extractEmacsLispBlocks(content);
    const errors: Array<{ block: number; error: any }> = [];
    let changes = 0;

    // 各ブロックを逆順で処理（行番号のずれを防ぐため）
    for (let i = blocks.length - 1; i >= 0; i--) {
      const block = blocks[i];
      const result = this.applyParinfer(block.content, mode);

      if (result.success) {
        if (result.text !== block.content) {
          // コードが変更された場合、元のファイルを更新
          const newBlockLines = result.text.split("\n");
          lines.splice(
            block.startLine + 1,
            block.endLine - block.startLine - 1,
            ...newBlockLines
          );
          changes++;
        }
      } else {
        errors.push({ block: i, error: result.error });
      }
    }

    return {
      content: lines.join("\n"),
      changes,
      errors
    };
  }
}

// CLIのメイン処理
async function main() {
  const args = parse(Deno.args, {
    string: ["mode"],
    boolean: ["in-place", "dry-run", "help", "verbose"],
    default: {
      mode: "smart",
      "in-place": false,
      "dry-run": false,
      help: false,
      verbose: false
    }
  });

  if (args.help || args._.length === 0) {
    console.log(`
org-parinfer - Org-mode Emacs Lisp Parinfer CLI

使用方法:
  deno run --allow-read --allow-write org-parinfer.ts [options] <file.org>

オプション:
  --mode <mode>    使用するParinferモード (indent, paren, smart)
                   デフォルト: smart
  --in-place       ファイルを直接編集する
  --dry-run        実際の変更を行わず、結果を表示するのみ
  --verbose        詳細な情報を表示
  --help           このヘルプを表示

モードの説明:
  indent   - インデントに基づいて括弧を調整
  paren    - 括弧に基づいてインデントを調整
  smart    - 文脈に応じて最適な調整を実行（推奨）

例:
  # ファイルを確認（変更なし）
  deno run --allow-read org-parinfer.ts --dry-run config.org

  # ファイルを直接編集
  deno run --allow-read --allow-write org-parinfer.ts --in-place config.org

  # インデントモードを使用
  deno run --allow-read --allow-write org-parinfer.ts --mode indent --in-place config.org

  # 詳細情報を表示
  deno run --allow-read org-parinfer.ts --verbose --dry-run config.org
`);
    Deno.exit(0);
  }

  const filePath = String(args._[0]);
  const mode = args.mode as "indent" | "paren" | "smart";
  const inPlace = args["in-place"];
  const dryRun = args["dry-run"];
  const verbose = args.verbose;

  // ファイルの存在確認
  try {
    await Deno.stat(filePath);
  } catch {
    console.error(`エラー: ファイル '${filePath}' が見つかりません。`);
    Deno.exit(1);
  }

  // ファイルの読み込み
  const content = await Deno.readTextFile(filePath);
  const processor = new OrgModeProcessor();

  // 処理実行
  if (verbose) {
    console.log(`処理中: ${filePath}`);
    console.log(`モード: ${mode}`);
    console.log(`Parinferバージョン: ${parinfer.version}`);
  }

  const result = processor.processOrgFile(content, mode);

  // エラーの報告
  if (result.errors.length > 0) {
    console.error("エラーが発生しました:");
    for (const { block, error } of result.errors) {
      console.error(`  ブロック ${block + 1}: ${error?.message || JSON.stringify(error)}`);
      if (verbose && error) {
        console.error(`    詳細: 行 ${error.lineNo + 1}, 位置 ${error.x}`);
        if (error.name) {
          console.error(`    タイプ: ${error.name}`);
        }
      }
    }
  }

  // 結果の表示
  if (verbose) {
    const blocks = processor.extractEmacsLispBlocks(content);
    console.log(`検出されたEmacs Lispブロック数: ${blocks.length}`);
    console.log(`修正されたブロック数: ${result.changes}`);
  } else {
    console.log(`${result.changes} 個のコードブロックが修正されました。`);
  }

  if (dryRun) {
    if (result.changes > 0) {
      console.log("\n--- 修正後の内容 (dry-run) ---");
      console.log(result.content);
    } else {
      console.log("変更はありません。");
    }
  } else if (inPlace) {
    if (result.changes > 0) {
      await Deno.writeTextFile(filePath, result.content);
      console.log(`ファイル '${filePath}' を更新しました。`);
    } else {
      console.log("変更はありません。");
    }
  } else {
    // 標準出力に出力
    console.log(result.content);
  }
}

// エラーハンドリング
if (import.meta.main) {
  try {
    await main();
  } catch (error) {
    console.error("エラー:", error.message);
    Deno.exit(1);
  }
}
