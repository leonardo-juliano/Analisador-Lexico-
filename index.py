import ply.lex as lex
from tkinter import *
import tkinter as tk
from tkinter import ttk
from tkinter import filedialog
from tkinter import filedialog as fd

# Palavras Reservadas da Linguagem 
reserved = {
   'SE' : 'SE',
   'SENAO' : 'SENAO',
   'ENQUANTO' : 'ENQUANTO',
   'IFSULDEMINAS':'IFSULDEMINAS',
   'PARA':'PARA',
   'VERDADEIRO':'VERDADEIRO',
   'FALSO':'FALSO',
   'MOSTRAAI':'MOSTRAAI',
   'FAZAI':'FAZAI',
}

# Lista para os nomes dos tokens
tokens = [
#Operadores Matemáticos
'OP_MAIS' ,        #+
'OP_MENOS' ,       #-
'OP_VEZES',        #*
'OP_DIVIDE',       #/
'OP_MODULO',       #%
#Operadores de Execução
'DOIS_PONTOS' ,         #:
'FINAL_LINHA',        #;
'VIRGULA',              #,
'PONTO',                #.
#Operadores de Impressão
'ASPAS',    #"
'COMENTARIO',   ##
#Operadores de Atribuição
'ATRIB_NEGACAO',          #~
'ATRIB_IGUAL',            #=
'ATRIB_MAIS_IGUAL',       #+=
'ATRIB_MENOS_IGUAL',      #-=
'ATRIB_VEZES_IGUAL',      #*=
'ATRIB_DIVIDE_IGUAL',     #/=
                                                     #Operadores Relacionais
'REL_MENOR',           #<
'REL_MAIOR',           #>
'REL_MENOR_IGUAL',     #<=
'REL_MAIOR_IGUAL',     #>=
'REL_DUPLO_IGUAL',     #==
'REL_DIFERENTE',       #!=
'REL_E',               #&
'REL_OU' ,             #|
                                                    #Operadores de Prioridade
'ABRE_PARENTESES',       #(
'FECHA_PARENTESES',      #)
'ABRE_COLCHETES',        #[
'FECHA_COLCHETES',       #]
'ABRE_CHAVES',           #{
'FECHA_CHAVES',          #}
                                                    #Identificadores
'INTEIRO',      #inteiro
'DOUBLE',       #double
'STRING',       #string
'CHAR',         #char
'VARIAVEL',     #variavel
'TESTE',

#para a criação dos RegEx (para verificar as compatibilidades) com o PLY,as verificações tem que ter uma "chamada" pelo token, é padrão
'IGNORE',      #Ignorar tabulação e espaço

'variavel_mf', #variavel mal formada
'numero_mf',   #numero mal formado
'string_mf',   #string mal formada

] + list(reserved.values()) #concateno com as palavras reservadas para verificação

#Regras de expressão regular (RegEx) para tokens simples
t_OP_MAIS    = r'\+'
t_OP_MENOS   = r'-'
t_OP_VEZES   = r'\*'
t_OP_DIVIDE  = r'/'
t_OP_MODULO = r'\%'

t_DOIS_PONTOS = r'\:'
t_FINAL_LINHA= r'\;'
t_VIRGULA = r'\,'
t_PONTO = r'\.'

t_ASPAS = r'\"'
t_COMENTARIO = r'\#.*'

t_IFSULDEMINAS = r'IFSULDEMINAS'
t_ENQUANTO = r'ENQUANTO'
t_SE = r'SE'
t_SENAO = r'SENAO'
t_PARA = r'PARA'
t_VERDADEIRO = r'VERDADEIRO'
t_FALSO = r'FALSO'
t_MOSTRAAI = r'MOSTRAAI'
t_FAZAI = r'FAZAI'

t_ATRIB_NEGACAO = r'\~'
t_ATRIB_IGUAL = r'\='
t_ATRIB_MAIS_IGUAL = r'\+\='
t_ATRIB_MENOS_IGUAL = r'\-\='
t_ATRIB_VEZES_IGUAL = r'\*\='
t_ATRIB_DIVIDE_IGUAL = r'\/\='

t_REL_MENOR = r'\<'
t_REL_MAIOR= r'\>'
t_REL_MENOR_IGUAL = r'\<\='
t_REL_MAIOR_IGUAL = r'\>\='
t_REL_DUPLO_IGUAL = r'\=\='
t_REL_DIFERENTE = r'\!\='
t_REL_E= r'\&'
t_REL_OU = r'\|'

t_ABRE_PARENTESES  = r'\('
t_FECHA_PARENTESES  = r'\)'
t_ABRE_COLCHETES = r'\['
t_FECHA_COLCHETES = r'\]'
t_ABRE_CHAVES = r'\{'
t_FECHA_CHAVES = r'\}'

t_ignore  = ' \t' #ignora espaço e tabulação

#Regras de expressão regular (RegEx) para stirngs mal formadas
def t_STRING(t):
    r'("[^"]*")'
    return t

def t_string_mf(t):
    r'("[^"]*)'
    return t

def t_variavel_mf(t):
    r'([0-9]+[a-z]+)|([@!#$%&*]+[a-z]+|[a-z]+\.[0-9]+|[a-z]+[@!#$%&*]+)'
    return t

def t_numero_mf(t):
    r'([0-9]+\.[a-z]+[0-9]+)|([0-9]+\.[a-z]+)|([0-9]+\.[0-9]+[a-z]+)'
    return t

def t_DOUBLE(t):
    r'([0-9]+\.[0-9]+)|([0-9]+\.[0-9]+)'
    return t

def t_INTEIRO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_VARIAVEL(t):
   r'[A-Za-z][A-Za-z_0-9]*'
   return t

#Defina uma regra para que seja possível rastrear o números de linha
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_OP_FINALLINHA(t):
    r';\s*(\w+)*'
    t.lexer.lineno += t.value.count('\n')
    t.type = 'FINAL_LINHA'
    t.value = t.value.strip(';')
    return t

#Regra de tratamento de erros
erroslexicos = []
def t_error(t):
    erroslexicos.append(t)
    t.lexer.skip(1)
    
erros = 0

#função padrão para adicionar as classificações dos tokens para ser impressa pelo compilador
def add_lista_saida(t,notificacao):
    saidas.append((t.lineno,t.lexpos,t.type,t.value, notificacao))

saidas = []

#Aqui começa a execução do TkInter
root = tk.Tk() #cria a janela
class Application():
    def __init__(self):
        self.root = root
        self.tela()
        self.frames_da_tela()
        self.botoes()
        self.Menus()
        root.mainloop()

    def limpa_telaentrada(self):
        self.codigo_entry.delete(1.0, END)
        for i in self.saida.get_children():
            self.saida.delete(i)
        saidas.clear()
        erroslexicos.clear()
        global erros
        erros = 0
        self.frame_1.update()
        self.frame_2.update()
        root.update()

    def tela(self):
        self.root.title("ANALISADOR LÉXICO")
        self.root.configure(background="white")
        self.root.geometry("1200x600")
        self.root.resizable(True, True)
        self.root.minsize(width=550, height=350)

    def frames_da_tela(self):
        self.frame_1 = Frame(self.root, bd=4, bg="#DCDCDC",highlightbackground="grey", highlightthickness=3)
        self.frame_1.place(relx=0.02, rely=0.07, relwidth=0.96, relheight=0.55)
        self.frame_2 = Frame(self.root, bd=4, bg="#DCDCDC",highlightbackground="grey", highlightthickness=3)
        self.frame_2.place(relx=0.02, rely=0.70, relwidth=0.96, relheight=0.20)

    def chama_analisador(self):
        columns = ('linha', 'posicao', 'token', 'lexema', 'notificacao')
        self.saida = ttk.Treeview(self.frame_2, height=5, columns=columns, show='headings')
        self.saida.heading("linha", text='Linha')
        self.saida.heading("posicao", text='Posicao referente ao inicio da entrada')
        self.saida.heading("token", text='Token')
        self.saida.heading("lexema", text='Lexema')
        self.saida.heading("notificacao", text='Palavra Reservada ?')

        data = self.codigo_entry.get(1.0, "end-1c")
        data.lower()
        lexer = lex.lex()
        lexer.input(data)
        # Tokenizar a entrada para passar para o analisador léxico
        for tok in lexer:
            global erros
            if tok.value == "SE" or tok.value == "SENAO" or tok.value == "ENQUANTO" or tok.value == "IFSULDEMINAS" or tok.value == "PARA" or tok.value == "VERDADEIRO" or tok.value == "FALSO" or tok.value == "MOSTRAAI" or tok.value == "FAZAI":
                max = (len(tok.value))
                if (max < 20):
                    if tok.value in reserved:
                       tok.type = reserved[tok.value]
                       add_lista_saida(tok, f"palavra reservada")
                    else:
                        add_lista_saida(tok, f" ")
                else:
                    erros+=1
                    add_lista_saida(tok, f"Tamanho da Variavel maior que o suportado")
            elif tok.value == "string_mf":
                erros+=1
                add_lista_saida(tok,f"string mal formada")
            elif tok.value == "variavel_mf":
                erros+=1
                add_lista_saida(tok,f"variavel mal formada")
            elif tok.value == "numero_mf":
                erros+=1
                add_lista_saida(tok,f"numero mal formado")
            elif tok.type == "INTEIRO":
                max = (len(str(tok.value)))
                if (max > 15):
                    erros+=1
                    add_lista_saida(tok,f"entrada maior que a suportada")
                else:
                    add_lista_saida(tok, f" ")
            elif tok.type == "VARIAVEL":
                max = (len(str(tok.value)))
                if (max > 15):
                    erros+=1
                    add_lista_saida(tok,f"entrada maior que a suportada")
                else:
                    add_lista_saida(tok, f" ")
            
            else:
                add_lista_saida(tok, f" ")
        if (saidas[0][3] != "IFSULDEMINAS"):
                erros += 1
                self.saida.insert('', tk.END, values="Algoritmo sem IFSULDEMINAS no início, condicao obrigatoria")
        for tok in erroslexicos:
            add_lista_saida(tok,f"Caracter Inválido nesta linguagem")

        tamerroslex = len(erroslexicos)
        if tamerroslex == 0 and erros == 0:
            self.saida.insert('', tk.END, values="Análise Léxica Concluída sem Erros")
        else:
            self.saida.insert('', tk.END, values="Erro Léxico")

        for retorno in saidas:
            self.saida.insert('', tk.END, values=retorno)

        self.saida.place(relx=0.001, rely=0.01, relwidth=0.999, relheight=0.95)

        self.scrollAnalise = ttk.Scrollbar(self.frame_2, orient='vertical',command=self.saida.yview)
        self.scrollAnalise.place(relx=0.979, rely=0.0192, relwidth=0.02, relheight=0.92)
        self.saida['yscrollcommand'] = self.scrollAnalise

    def botoes(self):

        # botao executar
        self.bt_executar = Button(text="Executar", bd=2, bg="lightgreen", font=('', 11), command=self.chama_analisador)
        self.bt_executar.place(relx=0.85, rely=0.92, relwidth=0.1, relheight=0.05)
                # criação da label da analise lexica
        self.lb_rodape = Label(text="Análise Léxica", bg="white", font=('', 12))
        self.lb_rodape.place(relx=0.001, rely=0.62, relwidth=0.2, relheight=0.07)

        # criação da label e entrada do código
        self.lb_codigo = Label(text="Código Fonte", bg="white", font=('', 12))
        self.lb_codigo.place(relx=0.001, rely=-0.001, relwidth=0.2, relheight=0.07)

        # criação da label da analise lexica
        self.lb_analise = Label(text="Análise Léxica", bg="white", font=('', 12))
        self.lb_analise.place(relx=0.001, rely=0.62, relwidth=0.2, relheight=0.07)

        self.codigo_entry = tk.Text(self.frame_1)
        self.codigo_entry.place(relx=0.001, rely=0.001, relwidth=0.995, relheight=0.995)

        self.scroll_bar = ttk.Scrollbar(self.frame_1, orient='vertical', command=self.codigo_entry.yview)
        self.scroll_bar.place(relx=0.982, rely=0.0019, relwidth=0.015, relheight=0.99)
        self.codigo_entry['yscrollcommand'] = self.scroll_bar


    def Menus(self):
        menubar = Menu(self.root)
        self.root.config(menu=menubar)

        def Quit(): self.root.destroy()

        def onOpen():
            tf = fd.askopenfilename(
                initialdir="C:/Users/MainFrame/Desktop/",
                title="Open Text file",
                filetypes=(("Text Files", "*.txt"),)
            )
            tf = open(tf, 'r')
            entrada = tf.read()
            self.codigo_entry.insert(END, entrada)
            tf.close()

        def onSave():
            files = filedialog.asksaveasfile(mode='w', defaultextension=".txt")
            t = self.codigo_entry.get(0.0, END)
            files.write(t.rstrip())

        menubar.add_command(label="Carregar código", command=onOpen)
        menubar.add_command(label="Salvar", command=onSave)
        menubar.add_command(label="Limpar", command=self.limpa_telaentrada)
        menubar.add_command(label="Sair", command=Quit)

Application()
