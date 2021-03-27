# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in,
# so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here:
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                   [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                   rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                   rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                   rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                   [[[0, 0], [-1, 0], [1, 0],[-2, 0], [2, 0]], # 5 long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, -2] [0, 2]]],
                   rotations([[0, 0], [1, 0], [0, 1], [1, 1], [0, 2]]), # square + tail
                   rotations([[0, 0], [0, 1], [1, 0]])] # corner
  # Your Enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end

end

class MyBoard < Board
  # Your Enhancements here:
  def initialize (game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheat = false
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    if !game_over? and @game.is_running? and @score > 99
      @score -= 100
      @cheat = true
    end
    draw
  end

  # gets the next piece
  def next_piece
    if (@cheat)
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    else
      @current_block = MyPiece.cheat_piece(self)
      @current_pos = nil
      @cheat = false
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.length()-1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # Your Enhancements here:
  # creates the window and starts the game
  def initialize
    super
    key_bind_u_c
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bind_u_c
    @root.bind('u' , proc {@board.rotate_180})
    @root.bind('c' , proc {@board.cheat})
  end

  class MyTetrisChallenge < MyTetris

  end

  class MyPieceChallenge < MyPiece

  end

  class MyBoardChallenge < MyBoard

  end

end
