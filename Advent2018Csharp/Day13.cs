using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection.Metadata.Ecma335;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace Tests
{
    public enum Track
    {
        Horizontal,
        Vertical,
        Slash,
        Backslash,
        Intersection,
        Empty,
    }

    public enum Direction
    {
        Up,
        Down,
        Left,
        Right,
    }

    public static class Extensions
    {
        public static Track ToTrack(this char c)
        {
            switch (c)
            {
                case '-': return Track.Horizontal;
                case '|': return Track.Vertical;
                case '/': return Track.Slash;
                case '\\': return Track.Backslash;
                case '+': return Track.Intersection;
                default: return Track.Empty;
            }
        }
        
        public static Direction? ToCartDirection(this char c)
        {
            switch (c)
            {
                case 'v': return Direction.Down;
                case '^': return Direction.Up;
                case '>': return Direction.Right;
                case '<': return Direction.Left;
                default: return null;
            }
        }
        
        public static (int x, int y) Step(this (int x, int y) pos, Direction dir)
        {
            switch (dir)
            {
                case Direction.Up:
                    return (pos.x, pos.y - 1);
                case Direction.Down:
                    return (pos.x, pos.y + 1);
                case Direction.Left:
                    return (pos.x - 1, pos.y);
                case Direction.Right:
                    return (pos.x + 1, pos.y);
                default:
                    return pos;
            }
        }

        public static Direction? Turn(this Track track, Direction dir)
        {
            switch (track)
            {
                case Track.Vertical:
                case Track.Horizontal:
                case Track.Empty:
                    return dir;
                case Track.Slash:
                    switch (dir)
                    {
                        case Direction.Up: return Direction.Right;
                        case Direction.Down: return Direction.Left;
                        case Direction.Left: return Direction.Down;
                        case Direction.Right: return Direction.Up;
                        default: return Direction.Right;
                    }
                case Track.Backslash:
                    switch (dir)
                    {
                        case Direction.Up: return Direction.Left;
                        case Direction.Down: return Direction.Right;
                        case Direction.Left: return Direction.Up;
                        case Direction.Right: return Direction.Down;
                        default: return Direction.Right;
                    }
                case Track.Intersection:
                    return null;
                default:
                    throw new ArgumentOutOfRangeException(nameof(track), track, null);
            }
        }

    }
    
    public class Day13
    {
        public class Cart
        {
            public Direction Direction { get; private set; }
            public (int x, int y) Position { get; private set; }

            public Cart((int x, int y) pos, Direction direction)
            {
                Direction = direction;
                Position = pos;
            }

            public void Next(IDictionary<(int,int),Track> dictionary)
            {
                var newPos = Position.Step(Direction);
                var track = dictionary[newPos];
                var newDirection = track.Turn(Direction) ?? TurnInIntersection();
                (Position, Direction) = (newPos, newDirection);
            }

            private int _intersection = 0;
            private Direction TurnInIntersection()
            {
                Direction dir;
                switch (Direction)
                {
                    case Direction.Up:
                        switch (_intersection)
                        {
                            case 0: 
                                dir = Direction.Left;
                                break;
                            case 1: 
                                dir = Direction.Up;
                                break;
                            default:
                                dir = Direction.Right;
                                break;
                        }
                        break;
                    case Direction.Down:
                        switch (_intersection)
                        {
                            case 0: 
                                dir = Direction.Right;
                                break;
                            case 1: 
                                dir = Direction.Down;
                                break;
                            default:
                                dir = Direction.Left;
                                break;
                        }
                        break;
                    case Direction.Right:
                        switch (_intersection)
                        {
                            case 0: 
                                dir = Direction.Up;
                                break;
                            case 1: 
                                dir = Direction.Right;
                                break;
                            default:
                                dir = Direction.Down;
                                break;
                        }
                        break;
                    default:
                        switch (_intersection)
                        {
                            case 0: 
                                dir = Direction.Down;
                                break;
                            case 1: 
                                dir = Direction.Left;
                                break;
                            default:
                                dir = Direction.Up;
                                break;
                        }
                        break;
                }

                _intersection = ++_intersection % 3;
                return dir;
            }
        }

        public static (IList<Cart>, IDictionary<(int,int),Track>) Parse(IEnumerable<string> lines)
        {
            var carts = new List<Cart>();
            var tracks = lines;

            IEnumerable<((int, int),char)> ZipLine(int rowNr, IEnumerable<char> row)
            {
                return Enumerable.Range(0, row.Count()).Zip(row, (col, track) => ((col, rowNr), track));
            }
            var zipped = Enumerable.Range(0, tracks.Count()).Zip(tracks, ZipLine);
            var singleList = zipped
                .SelectMany(i => i);
            var onlyTracks = singleList
                .Select(tup =>
                {
                    var pos = tup.Item1;
                    var c = tup.Item2;
                    var direction = c.ToCartDirection();
                    if (direction != null)
                    {
                        carts.Add(new Cart(pos, direction.Value));
                        if (direction == Direction.Up || direction == Direction.Down)
                        {
                            return (pos, Track.Vertical);
                        }
                        else
                        {
                            return (pos, Track.Horizontal);
                        }
                    }

                    return (pos, c.ToTrack());
                });
            var dict = onlyTracks.ToImmutableSortedDictionary(a => a.Item1, a => a.Item2);
            return (carts, dict);
        }

        public static (int x, int y) RunUntilCrash(IDictionary<(int, int), Track> tracks, IList<Cart> carts)
        {
            do
            {
                foreach (var cart in carts)
                {
                    cart.Next(tracks);
                    var collisionPosition = CheckCollision(carts);
                    if (collisionPosition != null)
                    {
                        return collisionPosition.First();
                    }
                }
            } while (true);
        }
        
        public static (int x, int y) RunUntilOnly1Cart(IDictionary<(int, int), Track> tracks, IList<Cart> initCarts)
        {
            var carts = initCarts;
            do
            {
                carts = carts.OrderBy(c => (c.Position.y, c.Position.x)).ToList();
                var toBeRemoved = new HashSet<Cart>();
                foreach (var cart in carts)
                {
                    if (toBeRemoved.Contains(cart)) { continue; }
                    cart.Next(tracks);
                    var collisionPosition = CheckCollision(carts);
                    if (collisionPosition != null)
                    {
                        // Remove carts at position
                        var collided = carts.Where(c => collisionPosition.Contains(c.Position)).ToList();
                        collided.ForEach(c => toBeRemoved.Add(c));
                    }
                }
                toBeRemoved.ToList().ForEach(c => carts.Remove(c));
            } while (carts.Count() > 1);

            return carts.First().Position;
        }

        private static IEnumerable<(int x, int y)> CheckCollision(IList<Cart> carts)
        {
            var collisions = carts
                .GroupBy(c => c.Position)
                .Where(g => g.Count() > 1)
                .Select(y => y.Key)
                .ToList();
            if (collisions.Any())
            {
                return collisions;
            }

            return null;
        }

        public static (int x, int y) Part1()
        {
            var file = "../../../../../../code/advent/input13.txt";
            var lines = System.IO.File.ReadLines(file);
            var (carts, tracks) = Parse(lines);
            var firstCrash = RunUntilCrash(tracks, carts);
            return firstCrash;
        }
        public static (int x, int y) Part2()
        {
            var file = "../../../../../../code/advent/input13.txt";
            var lines = System.IO.File.ReadLines(file);
            var (carts, tracks) = Parse(lines);
            var firstCrash = RunUntilOnly1Cart(tracks, carts);
            return firstCrash;
        }
    }
    public class Day13Test
    {
        [Test]
        public void ExamplePart1()
        {
            var lines = @"/->-\        
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   ".Split('\n');
            var (carts, tracks) = Day13.Parse(lines);
            var crash = Day13.RunUntilCrash(tracks, carts);
            Assert.AreEqual((7,3), crash);
        }
        
        [Test]
        public void ExamplePart2()
        {
            var lines = @"/>-<\  
|   |  
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/".Split('\n');
            var (carts, tracks) = Day13.Parse(lines);
            var crash = Day13.RunUntilOnly1Cart(tracks, carts);
            Assert.AreEqual((6,4), crash);
        }
        
        [Test]
        public void Part1()
        {
            var crash = Day13.Part1();
            Assert.AreEqual((69, 46), crash);
        }
        
        [Test]
        public void Part2()
        {
            var crash = Day13.Part2();
            Assert.AreEqual((118, 108), crash);
        }
    }
}