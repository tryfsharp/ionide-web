require.config({ paths: { 'vs': 'node_modules/monaco-editor/dev/vs' }});
require(['vs/editor/editor.main'], function() {
    var editor = monaco.editor.create(document.getElementById('container'), {
        value: [
            'let t = "Hello world!!!'
        ].join('\n'),
        language: 'fsharp'
    });
});