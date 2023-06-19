import ply.lex as lex
from ply import yacc
from cProfile import label
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
   'FALAI' : 'FALAI',
   'COMPILADORES':'COMPILADORES',
   'INICIO':'INICIO',
   'FIM':'FIM'
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
'ATRIB_IGUAL',  #=
'ATRIB_MAIS_IGUAL',       #+=
'ATRIB_MENOS_IGUAL',      #-=
'ATRIB_VEZES_IGUAL',      #*=
'ATRIB_DIVIDE_IGUAL', 


'OP_DIF_IGUAL',

'OP_RELACIONAL',
'PONTO_VIRGULA',

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
t_PONTO_VIRGULA = r'\;'



t_ASPAS = r'\"'
t_COMENTARIO = r'\#.*'

t_IFSULDEMINAS = r'IFSULDEMINAS'
t_INICIO = r'INICIO'
t_FIM = r'FIM'
t_COMPILADORES = r'COMPILADORES'
t_ENQUANTO = r'ENQUANTO'
t_SE = r'SE'
t_SENAO = r'SENAO'
t_PARA = r'PARA'
t_VERDADEIRO = r'VERDADEIRO'
t_FALSO = r'FALSO'
t_MOSTRAAI = r'MOSTRAAI'
t_FAZAI = r'FAZAI'
t_FALAI = r'FALAI'

t_ATRIB_NEGACAO = r'\~'
t_ATRIB_IGUAL = r'\='
t_OP_DIF_IGUAL = r'\!\='
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

#t_OP_RELACIONAL = t_REL_MENOR | t_REL_MAIOR | t_REL_MENOR_IGUAL | t_REL_MAIOR_IGUAL | t_REL_DUPLO_IGUAL | t_REL_DIFERENTE  | t_REL_E | t_REL_OU

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
   r'[a-z][a-z_0-9A-Z]*'
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


#analise sintatica 
def p_statements_multiple(p):
   '''
   statements : statements statement
   '''

def p_statements_single(p):
    '''
    statements : statement
    ''' 
def p_ifsuldeminas(p):
    '''
    statement : IFSULDEMINAS COMPILADORES INICIO comandos FIM
     '''
    
def p_comandos(p):
    '''
    comandos : comando 
    | comando comandos
    '''

def p_mostraai(p):
    '''
    comando : MOSTRAAI ABRE_PARENTESES possibilidades_mostraai FECHA_PARENTESES 
    '''

def p_falai(p):
    '''
    comando : VARIAVEL ATRIB_IGUAL FALAI ABRE_PARENTESES FECHA_PARENTESES
    '''

# def p_comentario(p):
#     '''
#     comando : COMENTARIO VARIAVEL
#     '''


def p_possibilidades_mostraai(p):
    '''
    possibilidades_mostraai : STRING 
    | INTEIRO 
    | VARIAVEL 
    | STRING possibilidades_mostraai 
    | INTEIRO possibilidades_mostraai 
    | VARIAVEL possibilidades_mostraai 
    '''

def p_declara_variavel(p):
    '''
    comando : declara_varias_variaveis
    '''

def p_declara_varias_variaveis(p):
    '''
    declara_varias_variaveis : VARIAVEL ATRIB_IGUAL mat_multi
    | VARIAVEL ATRIB_IGUAL mat_multi VIRGULA declara_varias_variaveis
    '''

def p_op_mat(p):
    '''
    op_matematica : OP_MAIS
    | OP_MENOS
    | OP_VEZES
    | OP_DIVIDE
    | OP_MODULO
    '''

def p_mat_mult(p):
    '''
    mat_multi : possibilidade_if_comparacao 
    | possibilidade_if_comparacao op_matematica mat_multi
    '''

def p_se(p):
    '''
    comando : SE ABRE_PARENTESES comparacao FECHA_PARENTESES ABRE_CHAVES comandos FECHA_CHAVES
    | SE ABRE_PARENTESES comparacao FECHA_PARENTESES ABRE_CHAVES comandos FECHA_CHAVES SENAO ABRE_CHAVES comandos FECHA_CHAVES 
    '''

def p_fazai(p):
    '''
    comando : FAZAI chamada_funcao ABRE_CHAVES comandos FECHA_CHAVES
    '''
def p_chama_fazai(p):
    '''
    comando : FAZAI PONTO VARIAVEL
    '''
def p_chamamada_funcao(p):
    '''
    chamada_funcao : VARIAVEL ABRE_PARENTESES FECHA_PARENTESES
    | VARIAVEL ABRE_PARENTESES possibilidade_if_comparacao FECHA_PARENTESES
    | VARIAVEL ABRE_PARENTESES possibilidade_if_comparacao VIRGULA possibilidade_if_comparacao FECHA_PARENTESES
    '''


def p_comparacao(p):
    '''
    comparacao : possibilidade_if_comparacao op_relacional possibilidade_if_comparacao 
    | possibilidade_if_comparacao op_relacional possibilidade_if_comparacao op_logico comparacao
    '''

def p_op_logico(p):
    '''
    op_logico : REL_E
    | REL_OU
    '''

def p_op_relacional(p):
    '''
    op_relacional : REL_MENOR
    | REL_MAIOR
    | REL_MENOR_IGUAL
    | REL_MAIOR_IGUAL
    | REL_DUPLO_IGUAL
    | REL_DIFERENTE
    '''

def p_possibilidade_if_comparacao(p):
    '''
    possibilidade_if_comparacao : VARIAVEL 
    | INTEIRO
    | STRING
    | VERDADEIRO
    | FALSO
    '''

errossintaticos = []
def p_error(p):
    errossintaticos.append(p)
    print("ERRO: ",p)

parser = yacc.yacc()

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
        self.root.title("ANALISADOR LÉXICO e SINTÁTICO")
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
        
        tamerroslex = len(erroslexicos)
        if tamerroslex == 0 and erros == 0:
            self.saida.insert('', tk.END, values="Análise Léxica Concluída sem Erros")
            parser.parse(data)
            tamerrosin = len(errossintaticos)
            print(errossintaticos)
            if tamerrosin == 0:
                self.saida.insert('', tk.END, values="Análise Sintática Concluída sem Erros")
            else:
                self.saida.insert('', tk.END, values="Erro Sintático")
        else:
            self.saida.insert('', tk.END, values="Erro Léxico")


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
        filemenu2 = Menu(menubar)

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

        def tokens():
            newWindow = Toplevel(root)
            newWindow.title("Tabela de Tokens")
            newWindow.configure(background="white")
            newWindow.geometry("800x800")
            newWindow.resizable(True, True)
            newWindow.minsize(width=550, height=350)

            newWindow = ttk.Treeview(newWindow, height=3, column=('col1', 'col2', 'col3', 'col4'))
            newWindow.heading("#0", text='')
            newWindow.heading("#1", text='Tokens')
            newWindow.heading("#2", text='Lexemas')
            newWindow.heading("#3", text='Expressão Regular')
            newWindow.heading("#4", text='Descrição')

            newWindow.column("#0", width=1, stretch=NO)
            newWindow.column("#1", width=50, )
            newWindow.column("#2", width=200)
            newWindow.column("#3", width=125)
            newWindow.column("#4", width=125)

            newWindow.place(relx=0.001, rely=0.01, relwidth=0.999, relheight=0.95)

            newWindow.insert("", 1, text="", values=("ifsuldeminas", "ifsuldeminas", "ifsuldeminas", "Palavra Reservada ifsuldeminas"))

            newWindow.insert("", 11, text="", values=("SE", "SE", "SE", "Palavra Reservada if"))
            newWindow.insert("", 12, text="", values=("elif", "elif", "elif", "Palavra Reservada elif"))
            newWindow.insert("", 13, text="", values=("SENAO", "SENAO", "SENAO", "Palavra Reservada else"))
            newWindow.insert("", 14, text="", values=("for", "for", "for", "Palavra Reservada for"))
            newWindow.insert("", 15, text="", values=("while", "while", "while", "Palavra Reservada while"))
            newWindow.insert("", 16, text="", values=("printf", "printf", "printf", "Palavra Reservada printf"))
            newWindow.insert("", 17, text="", values=("true", "true", "true", "Palavra Reservada true"))
            newWindow.insert("", 18, text="", values=("false", "false", "false", "Palavra Reservada false"))
            newWindow.insert("", 19, text="", values=("aux", "aux", "aux", "Palavra Reservada aux"))

            newWindow.insert("", 20, text="", values=("op_mat_mais", "+", "+", "Operador Matemático mais"))
            newWindow.insert("", 21, text="", values=("op_mat_menos", "-", "-", "Operador Matemático menos"))
            newWindow.insert("", 22, text="", values=("op_mat_vezes", "*", "*", "Operador Matemático vezes"))
            newWindow.insert("", 23, text="", values=("op_mat_divide", "/", "/", "Operador Matemático divide"))
            newWindow.insert("", 24, text="", values=("op_mat_modulo", "%", "%", "Operador Matemático modulo"))

            newWindow.insert("", 25, text="", values=("op_prio_abre_parenteses", "(", "(", "Operador de Prioridade abre parenteses"))
            newWindow.insert("", 26, text="", values=("op_prio_fecha_parenteses", ")", ")", "Operador de Prioridade fecha parenteses"))
            newWindow.insert("", 27, text="", values=("op_prio_abre_chaves", "{", "{", "Operador de Prioridade abre chaves"))
            newWindow.insert("", 28, text="", values=("op_prio_fecha_chaves", "}", "}", "Operador de Prioridade fecha chaves"))
            newWindow.insert("", 29, text="", values=("op_prio_abre_colchetes", "[", "[", "Operador de Prioridade abre colchetes"))
            newWindow.insert("", 30, text="", values=("op_prio_fecha_colchetes", "]", "]", "Operador de Prioridade fecha colchetes"))

            newWindow.insert("", 31, text="", values=("op_rel_menor", "<", "<", "Operador Relacional menor"))
            newWindow.insert("", 32, text="", values=("op_rel_maior", ">", ">", "Operador Relacional maior"))
            newWindow.insert("", 33, text="", values=("op_rel_menor_igual", "<=", "<=", "Operador Relacional menor igual"))
            newWindow.insert("", 34, text="", values=("op_rel_maior_igual", ">=", ">=", "Operador Relacional maior igual"))
            newWindow.insert("", 35, text="", values=("op_rel_duplo_igual", "==", "==", "Operador Relacional duplo igual"))
            newWindow.insert("", 36, text="", values=("op_rel_diferente", "!=", "!=", "Operador Relacional diferente"))
            newWindow.insert("", 37, text="", values=("op_rel_e", "&", "&", "Operador Relacional e"))
            newWindow.insert("", 38, text="", values=("op_rel_ou", "|", "|", "Operador Relacional ou"))

            newWindow.insert("", 39, text="", values=("inteiro", "0,1,2,3,4,5,6,7,8,9", "0|1|2|3|4|5|6|7|8|9", "Dígito Númerico Inteiro"))
            newWindow.insert("", 40, text="", values=("double", "0.009...9.9999", "0.00|9.999", "Dígito Númerico Double"))
            newWindow.insert("", 41, text="", values=("char", "a,b,c...x,y,z", "a|b|c...x|y|z", "Char"))
            newWindow.insert("", 42, text="", values=("variavel", "char(char,inteiro)*", "[char]{1}[char|inteiro]{*}", "Variável Criada"))
            newWindow.insert("", 43, text="", values=("string", "qualquer entrada de texto", "[char]{1}[char|inteiro]{*}", "Entrada do tipo string"))

            newWindow.insert("", 44, text="", values=("op_exec_virgula", ",", ",", "Operador de Execução Vírgula"))
            newWindow.insert("", 45, text="", values=("op_exec_ponto_virgula", ";", ";", "Operador de Execução ponto e vírgula"))
            newWindow.insert("", 46, text="", values=("op_exec_dois_pontos", ":", ":", "Operador de Execução dois pontos"))
            newWindow.insert("", 47, text="", values=("op_exec_ponto", ".", ".", "Operador de Execução ponto"))

            newWindow.insert("", 48, text="", values=("op_imp_aspas", "'", "'", "Operação de Impressão aspa"))

            newWindow.insert("", 49, text="", values=("op_comentario", "#", "#", "Operador de Comentário"))
            newWindow.insert("", 50, text="", values=("op_finallinha", "'", "'", "Operador de Final de Linha"))

            newWindow.insert("", 51, text="", values=("op_atrib_negacao", "~", "~", "Operador de Atribuição negação"))
            newWindow.insert("", 52, text="", values=("op_atri_igual", "=", "=", "Comando de Atribuição igual"))
            newWindow.insert("", 53, text="", values=("op_atri_mais_igual", "+=", "+=", "Comando de Atribuição mais igual"))
            newWindow.insert("", 54, text="", values=("op_atri_menos_igual", "-=", "-=", "Comando de Atribuição menos igual"))
            newWindow.insert("", 55, text="", values=("op_atri_vezes_igual", "*=", "*=", "Comando de Atribuição vezes igual"))
            newWindow.insert("", 56, text="", values=("op_atri_divide_igual", "/=", "/=", "Comando de Atribuição divide igual"))

            label.pack(pady=10)
            mainloop()

        menubar.add_command(label="Carregar código", command=onOpen)
        menubar.add_command(label="Salvar", command=onSave)
        menubar.add_command(label="Limpar", command=self.limpa_telaentrada)
        menubar.add_cascade(label="Tabela de Tokens", menu=filemenu2)
        menubar.add_command(label="Sair", command=Quit)
        filemenu2.add_command(label="Tokens", command=tokens)


Application()
