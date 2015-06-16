import wx
import todo_list

class TodoList(wx.Panel):
    def __init__(self, parent):
        wx.Panel.__init__(self, parent)
        wx.StaticText(self, label="TODO list :", pos=(20, 30))

        # TODO list
        self.listbox = wx.ListBox(self, size=(250, 250), pos=(100, 30))
        
        # Remove button
        button = wx.Button(self, label="REMOVE", pos=(350, 30))
        self.Bind(wx.EVT_BUTTON, self.remove_item, button)

        self.entry = wx.TextCtrl(self, pos=(100, 325))
        
        # Add button
        button = wx.Button(self, label="ADD", pos=(200, 325))
        self.Bind(wx.EVT_BUTTON, self.add_item, button)        

    def add_item(self, event):
        todo_list.add_item(self.entry.GetValue())
        self.refresh_list()

    def remove_item(self, event):
        todo_list.remove_item(self.listbox.GetStringSelection())
        self.refresh_list()

    def refresh_list(self):
        for _ in range(self.listbox.GetCount()):
            self.listbox.Delete(0)
        self.listbox.InsertItems(todo_list.get_todo_list(), 0)

app = wx.App(False)
frame = wx.Frame(None, size=(450, 400))
panel = TodoList(frame)
frame.Show()
app.MainLoop()
