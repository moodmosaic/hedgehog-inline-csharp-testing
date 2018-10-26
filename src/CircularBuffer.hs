--
-- Based on https://github.com/owickstrom/hedgehog-inline-java-testing/blob/d0e51bd6712ce548b175262718d25649c5280933/src/main
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StaticPointers #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CircularBuffer where

-- | F# is supported as well, if you import
--   import           Clr.Inline (Clr, ClrPtr(..), GCHandle(..), fsharp)
--
--   F# and C# can be mixed and matched as well in the same .hs file
--   import           Clr.Inline (Clr, ClrPtr(..), GCHandle(..), csharp, fsharp)
--
import           Clr.Inline (Clr, ClrPtr(..), GCHandle(..), csharp)
import           System.IO.Unsafe (unsafePerformIO)

-- | A Generic Circular Buffer in C#
--   http://www.blackwasp.co.uk/CircularBuffer.aspx
--
[csharp|
public class CircularBuffer<T>
{
    private readonly T[] buffer;
    private readonly int bufferSize;
    private readonly object syncLock = new object();

    private int head;
    private int tail;
    private int length;

    public CircularBuffer(int bufferSize)
    {
        this.buffer =
            new T[bufferSize];
        this.bufferSize =
            bufferSize;
        this.head =
            bufferSize - 1;
    }

    private int NextIndex(int index)
    {
        return (index + 1) % this.bufferSize;
    }

    public T Get()
    {
        lock (this.syncLock)
        {
            T dequeued =
                this.buffer[this.tail];
            this.tail =
                this.NextIndex(this.tail);
            this.length--;

            return dequeued;
        }
    }

    private bool IsFull => this.length == this.bufferSize;

    public void Put(T toAdd)
    {
        lock (this.syncLock)
        {
            this.head = this.NextIndex(this.head);
            this.buffer[this.head] = toAdd;
            if (this.IsFull)
                this.tail = this.NextIndex(this.tail);
            else
                this.length++;
        }
    }

    public T[] Buffer => this.buffer;

    public int Length => this.length;
}
|]

-- | Haskell wrapper of CircularBuffer<T> class.
--

new :: Int -> IO (Clr "CircularBuffer<int>")
new capacity =
  [csharp|
    CircularBuffer<int> {
      return new CircularBuffer<int>($capacity:int);
    }
  |]

put :: Clr "CircularBuffer<int>" -> Int -> IO ()
put x v =
  [csharp|
    ($x:CircularBuffer<int>).Put($v:int);
  |]

size :: Clr "CircularBuffer<int>" -> IO Int
size x =
  [csharp|
    int {
      return ($x:CircularBuffer<int>).Length;
    }
  |]

get :: Clr "CircularBuffer<int>" -> IO (Maybe Int)
get x = do
  Just <$> [csharp|
    int {
      return ($x:CircularBuffer<int>).Get();
    }
  |]

instance Eq (Clr "CircularBuffer<int>") where
  x == y = unsafePerformIO $ do
    [csharp|
      bool {
        var @this = $x:CircularBuffer<int>;
        var other = $y:CircularBuffer<int>;
        return System.Linq.Enumerable.SequenceEqual(
          @this.Buffer, other.Buffer);
      }
    |]
