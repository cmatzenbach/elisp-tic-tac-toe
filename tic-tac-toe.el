(defun tictactoe ()
	(interactive)
	(switch-to-buffer "tictactoe")
	(tictactoe-mode)
	(tictactoe-init))

(define-derived-mode tictactoe-mode special-mode "tic-tac-toe"
	(define-key tictactoe-mode-map (kbd "C-m") 'tictactoe-mark))

(defvar *tictactoe-board* nil
	"The board itself")

(defconst *tictactoe-size* 3
	"Height/length of the board")

(defvar *tictactoe-current-player* nil
	"The character representing the current player")

(defun tictactoe-init ()
	"Setup the new tictactoe game"
	(setq *tictactoe-board* (make-vector (* *tictactoe-size* *tictactoe-size*) ?\.))
	(setq *tictactoe-current-player* ?\X)
	(tictactoe-draw-board))

(defun tictactoe-draw-board ()
	(let ((inhibit-read-only t))
		(erase-buffer)
		(dotimes (row *tictactoe-size*)
			(dotimes (col *tictactoe-size*)
				(insert (tictactoe-get-square row col)))
			(insert "\n"))))

(defun tictactoe-get-square (row column)
	"Get the value in the (row, column) square"
	(elt *tictactoe-board* (+ column (* row *tictactoe-size*))))

(defun tictactoe-set-square (row column value)
	"Set provided value in the (row, column) square"
	(aset *tictactoe-board* (+ column (* row *tictactoe-size*)) value))

(defun tictactoe-mark ()
	"Mark the current square"
	(interactive)
	(let ((row (1- (line-number-at-pos)))
				(column (current-column)))
		(tictactoe-set-square row column *tictactoe-current-player*))
	(tictactoe-draw-board)
	(when (tictactoe-game-won)
		(message "Congrats! Player %c won!" *tictactoe-current-player*))
	(tictactoe-swap-player))

(defun tictactoe-swap-player ()
	"Change the current player"
	(setq *tictactoe-current-player*
				(if (char-equal *tictactoe-current-player* ?\X)
						?\O
					?\X)))

(defun tictactoe-game-won ()
	"Return t if game is won, nil otherwise"
	(or (tictactoe-diagonal-win)
			(tictactoe-row-win)
			(tictactoe-column-win)))

(defun tictactoe-diagonal-win ()
	(or (tictactoe-all-same-player (tictactoe-get-square 0 0)
																 (tictactoe-get-square 1 1)
																 (tictactoe-get-square 2 2))
			(tictactoe-all-same-player (tictactoe-get-square 0 2)
																 (tictactoe-get-square 1 1)
																 (tictactoe-get-square 2 0))))

(defun tictactoe-row-win ()
	(let ((has-won nil))
		(dotimes (row *tictactoe-size*)
			(when (tictactoe-all-same-player
						 (tictactoe-get-square row 0)
						 (tictactoe-get-square row 1)
						 (tictactoe-get-square row 2))
				(setq has-won t)))
		has-won))

(defun tictactoe-column-win ()
	(let ((has-won nil))
		(dotimes (column *tictactoe-size*)
			(when (tictactoe-all-same-player
						 (tictactoe-get-square 0 column)
						 (tictactoe-get-square 1 column)
						 (tictactoe-get-square 2 column))
				(setq has-won t)))
		has-won))

(defun tictactoe-all-same-player (sq1 sq2 sq3)
	(and (tictactoe-is-a-player sq1)
			 (char-equal sq1 sq2)
			 (char-equal sq2 sq3)))

(defun tictactoe-is-a-player (sq)
	(or (char-equal sq ?\X)
			(char-equal sq ?\O)))
