#@ MODIF string_utils Utilitai  DATE 23/05/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
# (AT YOUR OPTION) ANY LATER VERSION.                                                  
#                                                                       
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
#                                                                       
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.        
# ======================================================================
# RESPONSABLE COURTOIS M.COURTOIS

"""Module rassemblant des fonctions utilitaires de manipulations
de chaines de caractères
"""

import os


# existe aussi dans Noyau.N_types
# mais on ne souhaite pas que Noyau dépende d'Utilitai
def force_list(obj):
    """Retourne `obj` si c'est une liste ou un tuple,
    sinon retourne [obj,] (en tant que list).
    """
    if type(obj) not in (list, tuple):
        obj = [obj, ]
    return list(obj)


def clean_string(chaine):
    """Supprime tous les caractères non imprimables.
    """
    invalid = '?'
    txt = []
    for char in chaine:
        if ord(char) != 0:
            txt.append(char)
        else:
            txt.append(invalid)
    return ''.join(txt)

# existe dans asrun.mystring
def maximize_lines(l_fields, maxlen, sep):
    """Construit des lignes dont la longueur est au plus de `maxlen` caractères.
    Les champs sont assemblés avec le séparateur `sep`.
    """
    newlines = []
    while len(l_fields) > 0:
        cur = []
        while len(l_fields) > 0 \
              and len(sep.join(cur + [l_fields[0],])) <= maxlen:
            cur.append(l_fields.pop(0))
        newlines.append(sep.join(cur))
    newlines = [l for l in newlines if l != '']
    return newlines

def cut_long_lines(txt, maxlen, sep=os.linesep,
                   l_separ=(' ', ',', ';', '.', ':')):
    """Coupe les morceaux de `txt` (isolés avec `sep`) de plus de `maxlen`
    caractères.
    On utilise successivement les séparateurs de `l_separ`.
    """
    l_lines = txt.split(sep)
    newlines = []
    for line in l_lines:
        if len(line) > maxlen:
            l_sep = list(l_separ)
            line = cut_long_lines(line, maxlen, l_sep[0], l_sep[1:])
            line = maximize_lines(line, maxlen, l_sep[0])
            newlines.extend(line)
        else:
            newlines.append(line)
    # au plus haut niveau, on assemble le texte
    if sep == os.linesep:
        newlines = os.linesep.join(newlines)
    return newlines


def copy_text_to(text, files):
    """Imprime le texte dans les fichiers.
    """
    files = force_list(files)
    for f_i in files:
        assert type(f_i) in (str, file)
        if type(f_i) == file:
            fobj = file
        else:
            fobj = open(f_i, 'a')
            # should be closed automatically
        fobj.write(text)
        fobj.write(os.linesep)
        fobj.flush()
