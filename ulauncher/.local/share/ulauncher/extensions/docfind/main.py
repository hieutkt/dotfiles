""" Main Module """

import logging
import os
import textwrap
import subprocess
import mimetypes
import gi
from collections import Counter
gi.require_version('Gtk', '3.0')
# pylint: disable=import-error
from gi.repository import Gio, Gtk
from ulauncher.api.client.Extension import Extension
from ulauncher.api.client.EventListener import EventListener
from ulauncher.api.shared.event import KeywordQueryEvent
from ulauncher.api.shared.item.ExtensionResultItem import ExtensionResultItem
from ulauncher.api.shared.item.ExtensionSmallResultItem import ExtensionSmallResultItem
from ulauncher.api.shared.action.RenderResultListAction import RenderResultListAction
from ulauncher.api.shared.action.OpenAction import OpenAction
from ulauncher.api.shared.action.RunScriptAction import RunScriptAction
from ulauncher.api.shared.action.DoNothingAction import DoNothingAction
from ulauncher.api.shared.action.HideWindowAction import HideWindowAction

LOGGING = logging.getLogger(__name__)


class FileSearchExtension(Extension):
    """ Main Extension Class  """

    def __init__(self):
        """ Initializes the extension """
        super(FileSearchExtension, self).__init__()
        self.subscribe(KeywordQueryEvent, KeywordQueryEventListener())

    def search(self, query):
        """ Try with the default fd or the previously successful command """
        bin_name = 'rga'

        """ Searches for Files using ripgrep-all """
        cmd = [
            'timeout', '5s', 'ionice', '-c', '3',
            bin_name, query, self.preferences['base_dir'],
            # '--files-with-matches',
            '--vimgrep',
            '--ignore-case'
        ]

        LOGGING.info(cmd)

        process = subprocess.Popen(cmd,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)

        out, err = process.communicate()

        if err:
            self.logger.error(err)
            return []

        matches = out.split('\n'.encode())
        matches = list([_f.split(':'.encode())[0] for _f in matches if _f])  # remove empty lines
        matches = Counter(matches)
        # Sort by count
        matches = {k: v for k, v in sorted(matches.items(),
                                           key=lambda e: e[1], reverse=True)}

        result = []
        #get folder icon outside loop, so it only happens once
        file = Gio.File.new_for_path("/")
        icon_theme = Gtk.IconTheme.get_default()

        # pylint: disable=C0103
        for filepath, count in matches.items():
            filename = os.path.basename(filepath)
            if len(splits := filename.split(b") ", 1)) == 2:
                authoryear, title = splits
                authoryear = authoryear + b")"
            else:
                authoryear, title = filename.split(b" ", 1)

            authoryear = f"{count:<3} match{'es' if count > 1 else ''} — ".encode() + authoryear

            type_, encoding = mimetypes.guess_type(filepath.decode('utf-8'))

            if type_:
                file_icon = Gio.content_type_get_icon(type_)
                file_info = icon_theme.choose_icon(file_icon.get_names(), 128, 0)
                if file_info:
                    icon = file_info.get_filename()
                else:
                    icon = "images/icon.png"
            else:
                icon = "images/icon.png"

            result.append({'path': filepath,
                           'name': textwrap.fill(title.decode('utf-8'), 55),
                           'description': authoryear,
                           'icon': icon})
        return result

    def get_open_at_search_position(self, path, query):
        """ Returns open pdf file at query location"""
        cmd = f"evince '{path}' --find='{query}'"
        LOGGING.info(cmd)
        return RunScriptAction(cmd)


class KeywordQueryEventListener(EventListener):
    """ Listener that handles the user input """

    # pylint: disable=unused-argument,no-self-use
    def on_event(self, event, extension):
        """ Handles the event """
        items = []

        query = event.get_argument()

        if not query or len(query) < 3:
            return RenderResultListAction([
                ExtensionResultItem(
                    icon='images/icon.png',
                    name='Docfind',
                    description='Search for keyword in stored Zotero docmuments...',
                    on_enter=DoNothingAction())
            ])

        results = extension.search(query.strip())

        if not results:
            return RenderResultListAction([
                ExtensionResultItem(
                    icon='images/icon.png',
                    name='Docfind',
                    description=f'No Results found matching: {query}',
                    on_enter=HideWindowAction())
            ])

        items = []
        for result in results:
            items.append(
                ExtensionResultItem(
                    icon=result['icon'],
                    name=result['name'],
                    description=result['description'].decode("utf-8"),
                    on_enter=extension.get_open_at_search_position(result['path'].decode("utf-8"), query)
                    ))

        return RenderResultListAction(items)


if __name__ == '__main__':
    FileSearchExtension().run()
