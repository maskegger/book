# Word to Rmd Process

## Step 1
Set working directory in powershell

## Step 2
Use pandoc to convert docx to md

Run the following pandoc command in powershell (replace $file with actual file:
pandoc $file.docx -f docx -t markdown -s -o $file_convert.md

## Step 3
Clean the markdown output to remove escape characters

Run the following powershell commands (replace filenmae with the actual file):

$original_file = 'file_convert.md'
$destination_file =  'file_cleaned.md'
(Get-Content $original_file) | Foreach-Object {
    $_ -replace [regex]::Escape("\|"), "|" `
       -replace [regex]::Escape("\["), "[" `
       -replace [regex]::Escape("\]"), "]" `
       -replace [regex]::Escape("\@"), "@" `
       -replace [regex]::Escape("(<"), "(" `
       -replace [regex]::Escape(">)"), ")"
       -replace [regex]::Escape("] ("), "[regex]::Escape("](")"
    } | Out-File -encoding ASCII $destination_file

## Step 4
Commit file_convert.md and file_clean.md to main J-PAL repository in branch specific for that chapter (chapter branch)

## Step 5
Make a copy of file_clean.md called file.Rmd, commmit to chapter branch

## Step 6
Perform edits in R (e.g. edit tables, insert figures, etc)

## Step 7
Commit edits to chapter branch

## Step 8
Test build off chapter branch (can fork to Jim repository to avoid using main repository github page)