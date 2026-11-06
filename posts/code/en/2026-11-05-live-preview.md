---
title: A new live preview plugin for nvim
date: 2026-11-05
description: ""
tags: nvim
---

# A new live preview plugin for nvim

You might ask: there are already so many Markdown previewers for Neovim, why do you need a new one? The answer is: it is still not enough! I hope this post can convince you that there is a gap in the ecosystem right now and [live-preview.nvim](https://github.com/ailrk/live-preview.nvim) does fill the gap.


## Two axes

One way to think about Markdown editing software is to organize it on a coordinate system. The x-axis is **liveness**, and the y-axis is the **level of fidelity**. I find these two axes are enough to draw a broad outline.

<img src="images/df8dsa.png">

**Liveness** measures the level of real-time responsiveness of the preview. The left end of the axis is what I call a "compiler approach": you do all your editing completely in the editor and only check the rendered Markdown once in a while. A canonical compiler-style workflow is Pandoc combined with a text editor. Since Markdown is already designed for readability, this isn't too bad, but if we can pursue greater liveness, why not?

On the other end, you have full WYSIWYG editors like Typora, which completely merge editing and previewing. The preview is fully dynamic and truly "live." This is very convenient if you need to check your results constantly, but because WYSIWYG applications are very complex, it's hard to build one, and the editing experience usually isn't as good as that of a dedicated, focused editor.

There is a spectrum of programs that sit between these two extremes. First, we have live reload, which means the preview picks up file changes without requiring a manual refresh. An example of this is [yusukebe/gh-markdown-preview](https://github.com/yusukebe/gh-markdown-preview). However, this class of previewer doesn't track editor scrolling; you still have to manually align your place in the editor with the preview. [iamcco/markdown-preview.nvim](https://github.com/iamcco/markdown-preview.nvim) goes a step further by tracking scroll position in real time. Even so, it is still very far away from Typora. Unlike Typora, because the cursor isn't tracked, you cannot comfortably edit the Markdown file while looking directly at the preview.

The **fidelity axis** measures how closely the preview resembles the final document you intend to present. Most browser-based and WYSIWYG tools share the same high level of fidelity because they render the full document in real time. Some Markdown previewers instead render Markdown directly inside the editor—a class of tools I call "simulated." For example, [MeanderingProgrammer/render-markdown.nvim](https://github.com/MeanderingProgrammer/render-markdown.nvim) uses Neovim's conceal feature to simulate the final outcome. `render-markdown`'s simulated approach provides a lightweight, near-WYSIWYG experience while retaining the full power of your editor. However, if your Markdown documents rely heavily on images, complex links, or math equations, in-editor concealment simply isn't enough.

WYSIWYG editors like Typora are interesting because they achieve the highest levels of both liveness and fidelity, meaning you can edit content while looking at the rendered result simultaneously. The catch is that they force you out of your preferred editor. Text editors are deeply personal tools; once someone has invested heavily in mastering one, they want to edit everything inside it. Yet if we look for an editor-based solution, the highest level of liveness we currently find is `iamcco/markdown-preview.nvim`, which lacks cursor synchronization. This means you can't edit while relying on the preview window because your eyes will lose track of your context.

As a result, there is an awkward gap: people who want a near-WYSIWYG experience with high fidelity and cursor tracking cannot find a previewer that fits their workflow. In my opinion, the main feature missing from the ecosystem is a browser based previewer with **bidirectional cursor synchronization**. This is how does it look like in action:

<img src="images/udofvz.gif">

## Sync the Cursor

How do you achieve cursor synchronization? First let's see how does `iamcco/markdown-preview.nvim` do it with scroll sync. It comes with a `nodejs` server that servesas a proxy between nvim and the browser. It serves the compiled html to the browser while taking input from nvim's RPC with [neoclide/vim-node-rpc](https://github.com/neoclide/vim-node-rpc), a library that allows you to establish a 2 way communication channel with a node server. With websocket and `vim-node-rpc`, we can now create a bidrectional channel between nvim and the browser directly. To sync the scroll, we just need to track the current cursor line number on both side, when one side changes send it to another and make an update.

<img src="images/8dfg4z.png">

But how do we synchronize the cursor position in real time? Scroll position is a single number that we can calculate from the cursor line number, but cursor position is more complicated. Nvim cursor has a integer based cursor position indicates the line number and the character, but the browser technically doesn't have a cursor at all, everything is "drawn" freely on the "big canvas". Not to mention that there are markdown characters that should be ignored in the browser when computing the position.

<!--
    annotation
    block based
    quantized by character length
    strip markdown decorations
-->


## Make it Simpler

<!--
    don't need node in between
    build http/ws directly in nvim lua.
-->

## Can we do more?

<!--
    improve the interactivity.
    select, copy paste, mode change
-->
