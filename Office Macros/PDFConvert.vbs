Attribute VB_Name = "PDFConvert"
'Declare public variables to be used by functions without explicitly passing them to the function
Public strFolderpath As String 'Folder path chosen by user
Public iRow As Integer 'Excel row index
Public iCol As Integer 'Excel column index
Public flgCancel As Boolean
Public boResult As Boolean
Public iCounter As Integer 'Counts files to be converted
Public iConvertCnt As Integer 'Counts files that have been converted
Public objFSO As Object 'File System Object
Public objFolder As Object
Public objAcroApp As Acrobat.AcroApp
Public objAcroAVDoc As Acrobat.AcroAVDoc
Public objAcroPDDoc As Acrobat.AcroPDDoc
Public objJSO As Object

Function DoFolder(folder)
    Dim SubFolder As Object
    Dim ext As String
    Dim strFile As String
    For Each SubFolder In folder.SubFolders
        DoFolder SubFolder
    Next
    Dim File As Object
    For Each File In folder.Files
        ext = Right(File.Name, 4)
        If ext = ".pdf" Or ext = ".png" Or ext = ".tif" Or ext = ".jpg" Then
        
            'Create path for converted file
            strFile = Left(File.Path, Len(File.Path) - 4) & ".docx"
            
            If Dir(strFile) = "" Then
                'Open document in Acrobat
                boResult = objAcroAVDoc.Open(File.Path, "")
                
                'Get objects in order to convert file
                Set objAcroPDDoc = objAcroAVDoc.GetPDDoc
                Set objJSO = objAcroPDDoc.GetJSObject
                        
                'Create path for converted file
                strFile = Left(File.Path, Len(File.Path) - 4) & ".docx"
                        
                'Convert file to docx
                boResult = objJSO.SaveAs(strFile, "com.adobe.acrobat.docx")
                
                'Log saving attachment
                Application.Range("a" & iRow + 1).Value = folder.Name
                Application.Range("b" & iRow + 1).Value = File.Name
                Application.Range("c" & iRow + 1).Value = strFile
                
                'Increment row index
                'iRow = iRow + 1
                        
                'Create path for converted file
                'strFile = Left(File.Path, Len(File.Path) - 4) & ".txt"
                        
                'Convert file to docx
                'boResult = objJSO.SaveAs(strFile, "com.adobe.acrobat.plain-text")
                        
                'Close document in Acrobat
                boResult = objAcroAVDoc.Close(True)
                
                'Log saving attachment
                'Application.Range("a" & iRow + 1).Value = folder.Name
                'Application.Range("b" & iRow + 1).Value = File.Name
                'Application.Range("c" & iRow + 1).Value = strFile
                
                'Increment row index
                iRow = iRow + 1
                
                iConvertCnt = iConvertCnt + 1
                Application.StatusBar = CStr(iConvertCnt) + " of " + CStr(iCounter) + " files converted"
            End If
        End If
    Next
End Function

Function CntFile(folder)
    Dim SubFolder As Object
    For Each SubFolder In folder.SubFolders
        CntFile SubFolder
    Next
    Dim File As Object
    Dim strFile As String
    For Each File In folder.Files
        ext = Right(File.Name, 4)
        If ext = ".pdf" Or ext = ".png" Or ext = ".tif" Or ext = ".jpg" Then
            strFile = Left(File.Path, Len(File.Path) - 4) & ".docx"
            If Dir(strFile) = "" Then
                iCounter = iCounter + 1
            End If
        End If
    Next
End Function

Sub PDFConvert()
    Dim strCol As String
    Dim i As Long
    Dim lngCount As Long
    Dim flgMB As Integer
    
    On Error Resume Next
    
    'Clear Excel file
    Application.Range("A1:ZZ1000").Clear
    
    flgCancel = False
    
    'Call userform so users can set the export destination and input search terms
    frmPDFConvert.Show
    
    If flgCancel = True Then
        Exit Sub
    End If
    
    'Set the folder path chosen in the user form
    If Right(strFolderpath, 1) <> "\" Then
        strFolderpath = strFolderpath & "\"
    End If

    Set objFSO = CreateObject("Scripting.FileSystemObject")
    Set objFolder = objFSO.GetFolder(strFolderpath)
    
    iCounter = 0
    iConvertCnt = 0
    
    'Call file count function
    CntFile objFolder
    
    flgMB = MsgBox(CStr(iCounter) & " files will be converted. Is this OK?", vbOKCancel)
    
    If flgMB = 2 Then
        Exit Sub
    End If
            
    'Set counter for row location on Excel file
    iRow = 1
    
    Application.DisplayStatusBar = True
    
    'Create column names on log file
    Application.Range("a" & 1).Value = "Folder Name"
    Application.Range("b" & 1).Value = "File Name"
    Application.Range("c" & 1).Value = "Path to Converted File"
    Application.Range("a" & 1).Font.Bold = True
    Application.Range("b" & 1).Font.Bold = True
    Application.Range("c" & 1).Font.Bold = True

    'Set Adobe Acrobat objects (Used to convert image files)
    Set objAcroApp = CreateObject("AcroExch.App")
    Set objAcroAVDoc = CreateObject("AcroExch.AVDoc")
    objAcroApp.Show
    
    DoFolder objFolder
    
    MsgBox "PDF Conversion Complete"
ExitSub:
    
    'Clear objects
    Set objFSO = Nothing
    Set objFolder = Nothing
    Set objAcroApp = Nothing
    Set objAcroAVDoc = Nothing
    Set objAcroPDDoc = Nothing
    Set objJSO = Nothing
End Sub
