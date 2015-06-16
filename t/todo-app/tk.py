import sys
 
# This is needed to run Tkinter from the c-api
sys.argv = ['']

import Tkinter as tk
import todo_list

class todolist_tk(tk.Tk):
    def __init__(self,parent):
        tk.Tk.__init__(self,parent)
        self.parent = parent
        self.initialize()

    def initialize(self):
        tk.Label(self, text="TODO list:").pack()
        self.listbox = tk.Listbox(self)
        self.listbox.pack()
        tk.Label(self, text="TODO: ").pack()
        tk.Button(self, text="Remove", command=self.remove_item).pack()

        self.entry = tk.Entry(self)
        self.entry.pack()

        tk.Button(self, text="Add", command=self.add_item).pack()

    def add_item(self):
        #self.listbox.insert(tk.END, self.entry.get())
        todo_list.add_item(self.entry.get())
        self.refresh_list()

    def remove_item(self):
        #self.listbox.remove_item(self.listbox.selection)
        if self.entry.get() is not None:
            todo_list.remove_item(self.entry.get())
            self.refresh_list()

    def refresh_list(self):
        self.listbox.delete(0, tk.END)
        items = todo_list.get_todo_list()
        for item in items:
            self.listbox.insert(tk.END, item)

app = todolist_tk(None)
app.title('TODO list')
app.mainloop()
