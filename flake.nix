{
  description = "Try multiple Emacs versions; fetch only the nixpkgs you actually use";

  inputs = {
    nixpkgs-30_1.url = "github:NixOS/nixpkgs/bf9fa86a9b1005d932f842edf2c38eeecc98eef3"; # 30.1
    nixpkgs-29_4.url = "github:NixOS/nixpkgs/b58e19b11fe72175fd7a9e014a4786a91e99da5f"; # 29.4
    nixpkgs-29_3.url = "github:NixOS/nixpkgs/1afc5440469f94e7ed26e8648820971b102afdc3"; # 29.3
    nixpkgs-29_2.url = "github:NixOS/nixpkgs/20bc93ca7b2158ebc99b8cef987a2173a81cde35"; # 29.2
    nixpkgs-29_1.url = "github:NixOS/nixpkgs/160b762eda6d139ac10ae081f8f78d640dd523eb"; # 29.1
    nixpkgs-28_2.url = "github:NixOS/nixpkgs/09ec6a0881e1a36c29d67497693a67a16f4da573"; # 28.2
    nixpkgs-28_1.url = "github:NixOS/nixpkgs/994df04c3c700fe9edb1b69b82ba3c627e5e04ff"; # 28.1
    nixpkgs-27_2.url = "github:NixOS/nixpkgs/0343e3415784b2cd9c68924294794f7dbee12ab3"; # 27.2 (Linux/macOS)
    nixpkgs-27_1.url = "github:NixOS/nixpkgs/54c1e44240d8a527a8f4892608c4bce5440c3ecb"; # 27.1 (Linux only)
    nixpkgs-26_3.url = "github:NixOS/nixpkgs/e5b91d92a01178f9eecc0c7dd09a89e29fe9cc6f"; # 26.3 (Linux only)
  };

  outputs = { self, ... }@inputs:
  let
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = f:
      builtins.listToAttrs (map (system: { name = system; value = f system; }) systems);

    # 参照されたときにだけ nixpkgs を import する
    mkEmacs = nixpkgsInput: system:
      (import nixpkgsInput { inherit system; }).emacs;

    mkApp = nixpkgsInput: system:
      let pkg = mkEmacs nixpkgsInput system;
      in {
        type = "app";
        program = "${pkg}/bin/emacs";
      };
  in
  {
    packages = forAllSystems (system:
      let
        isLinux = builtins.elem system [ "x86_64-linux" "aarch64-linux" ];
      in
      {
        emacs-30_1 = mkEmacs inputs.nixpkgs-30_1 system;
        emacs-29_4 = mkEmacs inputs.nixpkgs-29_4 system;
        emacs-29_3 = mkEmacs inputs.nixpkgs-29_3 system;
        emacs-29_2 = mkEmacs inputs.nixpkgs-29_2 system;
        emacs-29_1 = mkEmacs inputs.nixpkgs-29_1 system;
        emacs-28_2 = mkEmacs inputs.nixpkgs-28_2 system;
        emacs-28_1 = mkEmacs inputs.nixpkgs-28_1 system;
        emacs-27_2 = mkEmacs inputs.nixpkgs-27_2 system;
      } // (if isLinux then {
        emacs-27_1 = mkEmacs inputs.nixpkgs-27_1 system;
        emacs-26_3 = mkEmacs inputs.nixpkgs-26_3 system;
      } else {})
    );

    apps = forAllSystems (system:
      let
        isLinux = builtins.elem system [ "x86_64-linux" "aarch64-linux" ];
      in
      {
        emacs-30_1 = mkApp inputs.nixpkgs-30_1 system;
        emacs-29_4 = mkApp inputs.nixpkgs-29_4 system;
        emacs-29_3 = mkApp inputs.nixpkgs-29_3 system;
        emacs-29_2 = mkApp inputs.nixpkgs-29_2 system;
        emacs-29_1 = mkApp inputs.nixpkgs-29_1 system;
        emacs-28_2 = mkApp inputs.nixpkgs-28_2 system;
        emacs-28_1 = mkApp inputs.nixpkgs-28_1 system;
        emacs-27_2 = mkApp inputs.nixpkgs-27_2 system;
      } // (if isLinux then {
        emacs-27_1 = mkApp inputs.nixpkgs-27_1 system;
        emacs-26_3 = mkApp inputs.nixpkgs-26_3 system;
      } else {})
    );

    # デフォルトは 30.1（これだけが強制評価される）
    defaultPackage = forAllSystems (system: mkEmacs inputs.nixpkgs-30_1 system);
    defaultApp     = forAllSystems (system: mkApp   inputs.nixpkgs-30_1 system);
  };
}

