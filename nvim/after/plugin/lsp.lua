local lsp = require('lsp-zero')

lsp.preset('recommended')
lsp.setup()

local cmp = require('cmp')

lsp.setup_nvim_cmp({
  mapping = lsp.defaults.cmp_mappings({
    ['<Tab>'] = cmp.mapping.confirm(),
  })
})

lsp.setup()
