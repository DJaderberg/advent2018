using System.Linq;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace Tests
{
    public class Day4
    {
        [Test]
        public void Day4Part1()
        {
            Regex regex = new Regex("[(?<time>.*)] (?<info>)");
            var file = "../../../../../../code/advent/input4.txt";
            var lines = System.IO.File.ReadLines(file)
                .ToList();
        }
    }
}
