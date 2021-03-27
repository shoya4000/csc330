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
    @cheat = 0
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
      @cheat = 1
    end
    draw
  end

  # gets the next piece
  def next_piece
    if (@cheat == 0)
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    else
      @current_block = MyPiece.cheat_piece(self)
      @current_pos = nil
      @cheat = 0
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

end

# Below is the challenge portion of the assignment

class MyTetrisChallenge < MyTetris
  def initialize
    super
    challenge_key_bindings
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoardChallenge.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def challenge_key_bindings
    @root.bind('Left', proc {@board.move_left2})

    @root.bind('Right', proc {@board.move_right2})

    @root.bind('Down', proc {@board.rotate_clockwise2})

    @root.bind('Up', proc {@board.rotate_counter_clockwise2})
  end
end

class MyPieceChallenge < MyPiece
  def initialize (point_array, board, piece)
    @all_rotations = point_array
    @rotation_index = (0..(@all_rotations.size-1)).to_a.sample
    @color = All_Colors.sample
    @piece = piece
    if (piece == 1)
      @base_position = [2, 0] # [column, row]
    else
      @base_position = [7, 0]
    end
    @board = board
    @moved = true
  end

  def self.next_piece (board, piece)
    MyPieceChallenge.new(All_My_Pieces.sample, board, piece)
  end

  def self.cheat_piece (board, piece)
    MyPieceChallenge.new([[[0, 0]]], board, piece)
  end

  def move (delta_x, delta_y, delta_rotation)
    # Ensures that the rotation will always be a possible formation (as opposed
    # to nil) by altering the intended rotation so that it stays
    # within the bounds of the rotation array
    moved = true
    potential = @all_rotations[(@rotation_index + delta_rotation) % @all_rotations.size]
    # for each individual block in the piece, checks if the intended move
    # will put this block in an occupied space
    potential.each{|posns|
      if (@piece == 1)
        if !(@board.empty_at([posns[0] + delta_x + @base_position[0],
                              posns[1] + delta_y + @base_position[1]]));
          moved = false;
        end
      else
        if !(@board.empty_at2([posns[0] + delta_x + @base_position[0],
                               posns[1] + delta_y + @base_position[1]]));
          moved = false;
        end
      end
    }
    if moved
      @base_position[0] += delta_x
      @base_position[1] += delta_y
      @rotation_index = (@rotation_index + delta_rotation) % @all_rotations.size
    end
    moved
  end
end

class MyBoardChallenge < MyBoard
  def initialize (game)
    super
    @current_block = MyPieceChallenge.next_piece(self, 1)
    @current_block2 = MyPieceChallenge.next_piece(self, 2)
    @cheat = 0
  end

  def run
    ran = @current_block.drop_by_one
    ran2 = @current_block2.drop_by_one
    if !ran && !ran2
      store_current
      if !game_over?
        next_piece
        next_piece2
      end
    end
    @game.update_score
    draw
  end

  def move_left2
    if !game_over? and @game.is_running?
      @current_block2.move(-1, 0, 0)
    end
    draw
  end

  def move_right2
    if !game_over? and @game.is_running?
      @current_block2.move(1, 0, 0)
    end
    draw
  end

  def move_right2
    if !game_over? and @game.is_running?
      @current_block2.move(1, 0, 0)
    end
    draw
  end

  # rotates the current piece clockwise
  def rotate_clockwise2
    if !game_over? and @game.is_running?
      @current_block2.move(0, 0, 1)
    end
    draw
  end

  def rotate_counter_clockwise2
    if !game_over? and @game.is_running?
      @current_block2.move(0, 0, -1)
    end
    draw
  end

  # drops the piece to the lowest location in the currently occupied columns.
  # Then replaces it with a new piece
  # Change the score to reflect the distance dropped.
  def drop_all_the_way
    if @game.is_running?
      ran = @current_block.drop_by_one
      ran2 = @current_block2.drop_by_one
      @current_pos1.each{|block| block.remove}
      @current_pos2.each{|block| block.remove}
      while ran
        @score += 1
        ran = @current_block.drop_by_one
      end
      while ran2
        @score += 1
        ran2 = @current_block2.drop_by_one
      end
      draw
      store_current
      if !game_over?
        next_piece
        next_piece2
      end
      @game.update_score
      draw
    end
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
      @current_block2.move(0, 0, 2)
    end
    draw
  end

  # gets the next piece
  def next_piece
    if (@cheat == 0)
      @current_block = MyPieceChallenge.next_piece(self, 1)
      @current_pos1 = nil
    else
      @current_block = MyPieceChallenge.cheat_piece(self, 1)
      @current_pos1 = nil
      @cheat = 0
    end
  end

  # gets the next piece
  def next_piece2
    if (@cheat == 0)
      @current_block2 = MyPieceChallenge.next_piece(self, 2)
      @current_pos2 = nil
    else
      @current_block2 = MyPieceChallenge.cheat_piece(self, 2)
      @current_pos2 = nil
      @cheat = 0
    end
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    locations2 = @current_block2.current_rotation
    displacement2 = @current_block2.position
    (0..(locations.length()-1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos1[index]
    }
    (0..(locations2.length()-1)).each{|index|
      current = locations2[index];
      @grid[current[1]+displacement2[1]][current[0]+displacement2[0]] =
      @current_pos2[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def empty_at (point)
    if !(point[0] >= 0 and point[0] < (num_columns.div(2)))
      return false
    elsif point[1] < 1
      return true
    elsif point[1] >= num_rows
      return false
    end
    @grid[point[1]][point[0]] == nil
  end

  def empty_at2 (point)
    if !(point[0] >= (num_columns.div(2)) and point[0] < num_columns)
      return false
    elsif point[1] < 1
      return true
    elsif point[1] >= num_rows
      return false
    end
    @grid[point[1]][point[0]] == nil
  end

  def draw
    @current_pos1 = @game.draw_piece(@current_block, @current_pos1)
    @current_pos2 = @game.draw_piece(@current_block2, @current_pos2)
  end
end
