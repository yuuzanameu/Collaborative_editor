import Editor from '@monaco-editor/react';
import * as monaco from "monaco-editor";


function MonacoEditor() {
    function handleChange(val: string | undefined, event: monaco.editor.IModelContentChangedEvent) {
        console.log(val, event)
    }
    return (
        <div style={
            {
                border: '2px solid black',
                width: `700px`
            }
        }>
            <Editor
                height="300px"
                width="700px"
                defaultLanguage="plaintext"
                defaultValue="A"
                onChange={handleChange}
            />
        </div>
    )
}

export default MonacoEditor
