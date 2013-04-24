require_relative './hw6provided'
require_relative './hw6assignment'

class MyTest  #< MiniTest::Unit::TestCase
    def setup
        @t = MyTetris.new
        @board = @t.instance_variable_get(:@board)
        @block = @board.instance_variable_get(:@current_block)
        puts "setup complete"
        test_initialization
    end
    def test_initialization
        puts @t.instance_of? MyTetris   
        puts @board.instance_of? MyBoard    
        puts @block.instance_of? MyPiece
    end
end

MyTest.new.setup
