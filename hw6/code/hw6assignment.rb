# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyTetris < Tetris
  # your enhancements here
    def set_board
        @canvas = TetrisCanvas.new
        #line that changed (problem 1).
        @board = MyBoard.new(self)
        @canvas.place(@board.block_size * @board.num_rows + 3,
                      @board.block_size * @board.num_columns + 6, 24, 80)
        @board.draw
    end

    #problem 1
    def key_bindings
        super
        @root.bind('u', proc {@board.rotate180})
        @root.bind('c', proc {@board.cheat})
    end
end

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
    All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
            rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
            [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
            [[0, 0], [0, -1], [0, 1], [0, 2]]],
            rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
            rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
            rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
            rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
            rotations([[0, 0], [-1, 0], [1, 0], [-1, 1], [0, 1]]), #1
            rotations([[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]]),  #2
            rotations([[0, 0], [1, 0], [0, 1]])]                    #3
    # your enhancements here

    def self.next_piece(board)
        MyPiece.new(All_My_Pieces.sample, board)
    end


end

class MyBoard < Board
    # your enhancements here

    def initialize (game)
        @grid = Array.new(num_rows) {Array.new(num_columns)}
        @current_block = MyPiece.next_piece(self)
        @score = 0
        @game = game
        @delay = 500
        @cheatPiece = false
    end

    def next_piece
        if(!@cheatPiece)
            @current_block = MyPiece.next_piece(self)
        else
            @current_block = MyPiece.new([[[0,0]]], self)
            @cheatPiece = false;
        end
        @current_pos = nil
    end

    def cheat
        #@game.draw_piece([[[0,0]]], @current_block)
        if @score >= 100 && !@cheatPiece
            @cheatPiece = true
            @score -=100
        end
    end

    #fixes bug that pieces always must be length 4
    def store_current
        locations = @current_block.current_rotation
        displacement = @current_block.position
        #yes, I come from c++
        index = 0
        locations.each do |current|
            @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
                @current_pos[index]
            index += 1
        end
        remove_filled
        @delay = [@delay - 2, 80].max
    end

    # problem 1
    def rotate180
        if !game_over? and @game.is_running?
            @current_block.move(0,0,2)
        end
        draw
    end
end
