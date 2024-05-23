return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        rust_analyzer = {
          filetypes = { "rust" },
          root_dir = require("lspconfig").util.root_pattern("Cargo.toml"),
          settings = {
            ["rust-analyzer"] = {
              cargo = {
                allFeatures = true,
              },
            },
          },
        },
      },
    },
  },
}
