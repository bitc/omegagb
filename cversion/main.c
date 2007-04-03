#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <sys/time.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "rom_image.h"
#include "machine.h"

void test1(int argc, char* argv[], RomImage* rom_image);

int main(int argc, char* argv[])
{
	if(argc < 2)
	{
		printf("%s <rom-filename>\n", argv[0]);
		return 0;
	}
	RomImage rom;

	if(load_rom_image(&rom, argv[1]) == -1)
	{
		printf("error\n");
		return -1;
	}

	test1(argc, argv, &rom);

	free_rom_image(&rom);

	return 0;
}

/*
static void hello(GtkWidget *widget, gpointer data)
{
	g_print("Hello World\n");
}
*/

static void destroy(GtkWidget *widget, gpointer data)
{
	gtk_main_quit();
}

static void display_size_request(GtkWidget* widget, GtkRequisition* requisition, gpointer data)
{
	requisition->width = 160;
	requisition->height = 144;
}

struct DisplayExposeData
{
	GtkDrawingArea* display;
	GdkPixbuf* displayPixBuf;
	guchar* pbData;
	int row;
	int chan;
	Machine* machine;
	u8* breakpoint_table;
};

static int update_canvas(GtkDrawingArea* canvas, GdkPixbuf* pb, GdkEventExpose* expose);

static gboolean display_expose(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
	struct DisplayExposeData* d = data;
	return update_canvas(d->display, d->displayPixBuf, event);
}

void refresh_display(struct DisplayExposeData* display_expose_data, Machine* machine)
{
	int x, y;
	for(y = 0; y < 144; y++)
	{
		for(x = 0; x < 160; x++)
		{
			u8 color = machine->display[y * LCD_WIDTH + x];
			guchar color_byte = color * 85;
			display_expose_data->pbData[x*display_expose_data->chan +
				(y * display_expose_data->row) + 0] = color_byte;
			display_expose_data->pbData[x*display_expose_data->chan +
				(y * display_expose_data->row) + 1] = color_byte;
			display_expose_data->pbData[x*display_expose_data->chan +
				(y * display_expose_data->row) + 2] = color_byte;

			/*
			int yrow = y / 8;
			int xrow = x / 8;
			int tile_num = yrow * 32 + xrow;
			int tile_start_mem = 0x8000 + (16 * tile_num);
			int xoff = 7 - (x % 8);
			int yoff = y % 8;
			int hi_byte = tile_start_mem + (yoff * 2);
			int lo_byte = tile_start_mem + (yoff * 2) + 1;
			int hi_byte_value = machine->memory->ram[hi_byte];
			int lo_byte_value = machine->memory->ram[lo_byte];
			int color = 2 * !!(lo_byte_value & (1 << xoff)) +
				!!(hi_byte_value & (1 << xoff));
			guchar color_byte = color * 85;
			display_expose_data->pbData[x*display_expose_data->chan +
				(y * display_expose_data->row) + 0] = color_byte;
			display_expose_data->pbData[x*display_expose_data->chan +
				(y * display_expose_data->row) + 1] = color_byte;
			display_expose_data->pbData[x*display_expose_data->chan +
				(y * display_expose_data->row) + 2] = color_byte;
			*/
		}
	}

	gtk_widget_queue_draw(GTK_WIDGET(display_expose_data->display));
}

gboolean step(gpointer data)
{
	struct DisplayExposeData* d = data;

	update_machine_vblank(d->machine, d->breakpoint_table);

	refresh_display(d, d->machine);

	struct timeval tv;
	gettimeofday(&tv, 0);

	long long s = tv.tv_sec;
	long long u = tv.tv_usec;
	printf("%lld %lld\n", s, u);

	return TRUE;
}

void test1(int argc, char* argv[], RomImage* rom_image)
{
	Memory memory;
	if(init_memory(&memory, rom_image) == -1)
	{
		fprintf(stderr, "*** Memory Error\n");
		return;
	}
	Cpu cpu;
	Machine machine;
	init_machine(&machine, &cpu, &memory);

	gtk_init(&argc, &argv);

	GtkWidget* window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(destroy), 0);

	/*
	GtkWidget* button = gtk_button_new_with_label("Hello World!");
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(hello), 0);

	gtk_container_add(GTK_CONTAINER(window), button);
	gtk_widget_show(button);
	*/

	GtkWidget* display = gtk_drawing_area_new();
	GdkPixbuf* displayPixBuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, 160, 144);

	struct DisplayExposeData display_expose_data;
	display_expose_data.display = GTK_DRAWING_AREA(display);
	display_expose_data.displayPixBuf = displayPixBuf;
	display_expose_data.pbData = gdk_pixbuf_get_pixels(displayPixBuf);
	display_expose_data.row = gdk_pixbuf_get_rowstride(displayPixBuf);
	display_expose_data.chan = gdk_pixbuf_get_n_channels(displayPixBuf);
	display_expose_data.machine = &machine;
	display_expose_data.breakpoint_table = malloc(BREAKPOINT_TABLE_SIZE);
	clear_all_breakpoints(display_expose_data.breakpoint_table);
	/*
	set_breakpoint(display_expose_data.breakpoint_table, 0x2680);
	*/

	g_signal_connect(G_OBJECT(display), "size-request", G_CALLBACK(display_size_request), 0);
	g_signal_connect(G_OBJECT(display), "expose-event", G_CALLBACK(display_expose), &display_expose_data);


	gtk_container_add(GTK_CONTAINER(window), display);
	gtk_widget_show(display);
	
	gtk_widget_show(window);

	g_idle_add_full(G_PRIORITY_DEFAULT_IDLE, step, &display_expose_data, 0);

	gtk_main();

	free(display_expose_data.breakpoint_table);

	free_machine(&machine);
	free_memory(&memory);
}

static gboolean update_canvas(GtkDrawingArea* canvas, GdkPixbuf* pb, GdkEventExpose* expose)
{
	GdkDrawable* win = GTK_WIDGET(canvas)->window;
	GdkGC* gc = gdk_gc_new(win);
	int width = gdk_pixbuf_get_width(pb);
	int height = gdk_pixbuf_get_height(pb);
	GdkRectangle regionrect;
	regionrect.x = 0;
	regionrect.y = 0;
	regionrect.width = width;
	regionrect.height = height;
	GdkRegion* pbregion = gdk_region_rectangle(&regionrect);
	gdk_region_intersect(expose->region, pbregion);
	GdkRectangle* rects;
	gint n_rects;
	gdk_region_get_rectangles(expose->region, &rects, &n_rects);
	int i;
	for(i = 0; i < n_rects; i++)
	{
		gdk_draw_pixbuf(win, gc, pb,
				rects[i].x, rects[i].y,
				rects[i].x, rects[i].y,
				rects[i].width, rects[i].height,
				GDK_RGB_DITHER_NONE, 0, 0);
	}
	g_free(rects);
	gdk_gc_unref(gc);

	return TRUE;
}

