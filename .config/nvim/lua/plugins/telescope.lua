-- plugins/telescope.lua:
return {
    'nvim-telescope/telescope.nvim', tag = '0.1.8',
    cmd = "Telescope",
    version = false, -- telescope did only one release, so use HEAD for now
    keys = {
      {
        "<leader>fb",
        "<cmd>Telescope buffers sort_mru=true sort_lastused=true ignore_current_buffer=true<cr>",
        desc = "Buffers",
      },
      { "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Telescope buffers" },
      { "<leader>ff", "<cmd>Telescope find_files hidden=true no_ignore=false<cr>", desc = "Telescope find files"},
      { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Telescope live grep"},
    },
}

