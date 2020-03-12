using System;
using System.Drawing;
using System.Diagnostics;
using System.Timers;
using Console = Colorful.Console;
namespace TerminalView
{
    public class TerminalView
    {
        private int _width = 100;
        public int Width
        {
            get => _width;
            set
            {
                _width = value;
                UpdateSize(_width, _height);
            }
        }

        private int _height = 100;
        public int Height
        {
            get => _height;
            set
            {
                _height = value;
                UpdateSize(_width, _height);
            }
        }

        private int _fps = 30;
        private Timer _timer;

        public int Fps
        {
            get => _fps;
            set
            {
                var intervar = 1000 / value;

                if (this._timer == null)
                {
                    this._timer = new Timer { AutoReset = false };
                    this._timer.Elapsed += (sender, e) => { UpdateConsole(); };
                }

                this._timer.Interval = intervar;
            }
        }

        private Cell[,] internalBuffer;

        private void UpdateSize(int width, int height)
        {
            this.internalBuffer = new Cell[width, height];
            Console.SetWindowSize(width, height);
            Console.Clear();
        }

        private void UpdateConsole()
        {
            Console.SetWindowPosition(0, 0);
            foreach (var cell in this.internalBuffer)
            {
                var index = Array.IndexOf(this.internalBuffer, cell);
                var y = this.internalBuffer.Length / Width;
                var x = index - y * Width;

                cell.Draw(x, y);
            }
        }

        public void Start()
        {
            if (this._timer == null)
            {
                Fps = 30;
            }

            Console.Clear();
            UpdateConsole();
            this._timer.Start();
        }

        public void UpadateCell(Cell cell, Point point)
        {
            Debug.Assert((point.Y * this.Width + point.X) < this.internalBuffer.Length);
            this.internalBuffer[point.X, point.Y] = cell;
        }

        public void UpdateCells(Cell[] cells, Point from, int length)
        {
            Debug.Assert((from.Y * this.Width + from.X + length) < this.internalBuffer.Length);

            var fromIndex = from.Y * this.Width + from.X;
            cells.CopyTo(this.internalBuffer, fromIndex);
        }
    }
}
