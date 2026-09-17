# class terminal_block {#class-terminal_block}

One command a client ran in a `terminal_image`, as its OSC 133 _semantic
prompt_ marks describe it: the prompt (`A`), the line the user edited
(`B`), the output of running it (`C`) and the end of that output (`D`,
or the prompt after it).  The terminal builds them as the marks arrive
and keeps them in `<-blocks`, oldest first.  Subclass of `object`.

A block is what lets the window be addressed by what it was used for
rather than by row and column: `<-content` hands over the command or its
output as a string, `->select` and `->copy` put either on the clipboard,
`->scroll_to` brings it back into view and `->fold` takes its output off
the display.

A command can take more than one line to type.  The client asks for each
of them separately -- a Prolog term is read a line at a time until its
full stop -- and marks all but the first with `A;k=s`, the secondary
prompt.  Those lines are one command and one block, anchored to the line
it started on.

The commandline editor of SWI-Prolog emits the marks by default; see
`el_set/2` `prompt_marks(Boolean)` in `library(editline)`.  A client that
marks nothing produces no blocks.

## Lifetime {#class-terminal_block-lifetime}

`->remove` is the one thing that changes the buffer rather than reading
it: everything else here only says what is already there.

A block outlives the marks it was made from but not the text it points
at.  Its positions are lines of the scrollback, so a line pushed out of
the buffer takes with it the marks that named it, and a block with
nothing left is dropped from `<-blocks` with its `<-terminal` set to
`@nil`.  A block still held elsewhere is then harmless: every method
that needs text fails.

`<-prompt`, `<-input`, `<-output` and `<-end` report positions in the
flat character space of the terminal, which composes with `<-contents`,
`<-find`, `->selection` and `->scroll_to` -- but see *Character indices*
in the documentation of class `terminal_image`: those indices shift as
output pushes lines out of the scrollback.  `<-id` is the handle that
outlives them.

@see class terminal_image

## Instance variables {#class-terminal_block-instvars}

- terminal_block<-terminal: terminal_image*
    Terminal the block belongs to, or `@nil` once its lines have left
    the scrollback.

- terminal_block<-id: int
    Handle that outlives the character indices.  See `<-block`.

- terminal_block<->folded: bool
    Whether its output is collapsed.  Assigning `@on` folds and `@off`
    unfolds; it fails rather than folding a command still running, one
    that printed nothing, or anything while an application holds the
    alternate screen.

- terminal_block<-running: bool
    The command has been entered and is still writing its output, i.e.
    `C` was seen and `D` was not.

## Send methods {#class-terminal_block-send}

- terminal_block->initialise: terminal=terminal_image
    Create a block on a terminal.  Blocks are made by the terminal as
    the marks arrive; there is normally no reason to make one.

- terminal_block->select: what=[{command,output,all}]
    Make the block the selection of its terminal.  `command` is the line
    the user entered, `output` (the default) what it printed, and `all`
    both with the prompt in front.

- terminal_block->copy: what=[{command,output,all}], which=[{primary,clipboard}]
    Put the text of the block on the clipboard.  It goes by `<-content`
    rather than by the region of the screen the block covers, so a copy
    of the `command` is what was typed: without the return that entered
    it, and without the continuation prompts the client drew down the
    left of a command it collected over several lines.  `all` keeps
    them, being what the window shows.  Lines are separated by a single
    newline, as `<-content` separates them, and not by the `\r\n` of
    `terminal_image<-selected`: what is copied here is a command to be
    read back, not a region of the screen.  Does not disturb the
    selection; `->select` is for that.

- terminal_block->scroll_to
    Scroll the prompt of the block into view, opening a fold that hides
    it.

- terminal_block->remove
    Take the command out of the buffer altogether: the lines it covers
    go and the text under them moves up into the gap.  The block is
    dropped from `<-blocks` with its `<-terminal` set to `@nil`.  There
    is no undo.

    The buffer gets shorter at the end the client is writing to, so the
    older history is left where it is.  Everything that names a line by
    its place in the scrollback is moved with the text: the caret, the
    window and the marks of every other command.  The selection and a
    running incremental search are ended rather than followed, since what
    they were on may be what is going.

    Fails for a command still running, for one whose output has not been
    marked as ended, and while an application holds the alternate screen.

- terminal_block->fold
- terminal_block->unfold
- terminal_block->toggle_fold
    Hide the output of the command, show it again, or the one or the
    other.  Wrappers over `->folded`.

## Get methods {#class-terminal_block-get}

- terminal_block<-prompt: -> int
    Index where the prompt starts (OSC 133 `A`).  Fails for a read that
    printed no prompt, and once the line has left the buffer.

- terminal_block<-input: -> int
    Index where the line the user entered starts (OSC 133 `B`).

- terminal_block<-output: -> int
    Index where its output starts (OSC 133 `C`).  Fails while the line
    is still being edited.

- terminal_block<-end: -> int
    Index where its output ends (OSC 133 `D`).  Fails while the command
    is still running.

- terminal_block<-content: what=[{command,output,all}] -> string
    New string with the text of the block; `what` as for `->select`.
    Lines are separated by a single newline, as `<-contents` does.  A
    part that has not been reached yet -- the output of a command still
    running -- reaches to where the client is writing.

    `command` is what was typed: it stops short of the return that
    entered it, and passes over the continuation prompt in front of each
    line of a command the client collected over several.  `all` keeps
    both, being the text of the window.
