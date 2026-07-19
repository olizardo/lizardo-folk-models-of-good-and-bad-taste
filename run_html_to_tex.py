from html.parser import HTMLParser
import glob
import os

class TableParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.in_table = False
        self.in_tr = False
        self.in_th_td = False
        self.current_cell = []
        self.current_row = []
        self.rows = []
        
    def handle_starttag(self, tag, attrs):
        if tag == 'table':
            self.in_table = True
        elif tag == 'tr' and self.in_table:
            self.in_tr = True
            self.current_row = []
        elif tag in ('th', 'td') and self.in_tr:
            self.in_th_td = True
            self.current_cell = []

    def handle_endtag(self, tag):
        if tag == 'table':
            self.in_table = False
        elif tag == 'tr' and self.in_tr:
            self.in_tr = False
            self.rows.append(self.current_row)
        elif tag in ('th', 'td') and self.in_th_td:
            self.in_th_td = False
            cell_text = "".join(self.current_cell).strip().replace('&', '\\&').replace('%', '\\%').replace('_', '\\_')
            self.current_row.append(cell_text)

    def handle_data(self, data):
        if self.in_th_td:
            self.current_cell.append(data)

html_files = glob.glob('report/Tabs/*.html')
for file in html_files:
    try:
        with open(file, 'r', encoding='utf-8') as f:
            content = f.read()
            
        parser = TableParser()
        parser.feed(content)
        
        if not parser.rows:
            continue
            
        tex_file = file.replace('.html', '.tex')
        with open(tex_file, 'w', encoding='utf-8') as f:
            num_cols = len(parser.rows[0])
            col_format = 'l' * num_cols
            f.write('\\begin{table}[htpb]\n')
            f.write('\\centering\n')
            f.write('\\resizebox{\\textwidth}{!}{\n')
            f.write('\\begin{tabular}{' + col_format + '}\n')
            f.write('\\toprule\n')
            
            for i, row in enumerate(parser.rows):
                f.write(' & '.join(row) + ' \\\\\n')
                if i == 0:
                    f.write('\\midrule\n')
                    
            f.write('\\bottomrule\n')
            f.write('\\end{tabular}\n')
            f.write('}\n')
            # generate caption from filename
            caption = os.path.basename(file).replace('_', ' ').replace('.html', '').title()
            f.write(f'\\caption{{{caption}}}\n')
            # generate label
            label = os.path.basename(file).replace('.html', '')
            f.write(f'\\label{{tab:{label}}}\n')
            f.write('\\end{table}\n')
            
        print(f"Converted {file} to {tex_file}")
    except Exception as e:
        print(f"Failed to convert {file}: {e}")