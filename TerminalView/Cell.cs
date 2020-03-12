using System;
using System.Drawing;
using Console = Colorful.Console;

namespace TerminalView
{
    public class Cell
    {
        public Color Color { get; set; } = Color.Black;
        public Char Symbol { get; set; } = ' ';

        public void Draw(Point point)
        {
            Draw(point.X, point.Y);
        }

        public void Draw(int x, int y)
        {
            Console.SetWindowPosition(x, y);
            Console.Write(Symbol, Color);
        }

        public void Draw()
        {
            Console.Write(Symbol, Color);
        }
    }
}
