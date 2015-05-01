# coding=utf-8

# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
'''
On regroupe ici les fonctions Python necessaires au lancement
de la fenetre graphique d'appariement manuel des MAC pour le
recalage en dynamique
'''
import numpy as NP


def extract_mac_array(mac_mode):

    data1 = mac_mode.EXTR_TABLE().Array('NUME_MODE_1', 'MAC')
    data2 = mac_mode.EXTR_TABLE().Array('NUME_MODE_2', 'MAC')

    N = int(NP.maximum.reduce(data1[:, 0]))
    M = int(NP.maximum.reduce(data2[:, 0]))
    mac = NP.zeros((N, M))
    for i in range(data1.shape[0]):
        i1 = int(data1[i, 0]) - 1
        i2 = int(data2[i, 0]) - 1
        mac[i1, i2] = data1[i, 1]
    return mac


def get_modes(resu):

    afreq = resu.LIST_PARA()['FREQ']

    return afreq


class fenetre_mac:

    def __init__(self, resu1, resu2, mac):
        from Calc_essai.outils_ihm import MacWindowFrame
        from Tkinter import Tk, Frame, StringVar, Entry, Label, Button

        self.resu1 = resu1
        self.resu2 = resu2
        self.mac = mac
        self.root = Tk()

        nom1 = resu1.nom
        nom2 = resu2.nom
        titre = "MAC pour la base " + nom1 + " et " + nom2
        size = (20, 300)

        # la fenetre de MAC
        mac_win = MacWindowFrame(self.root, titre, nom1, nom2, size)
        mac_win.grid(row=0, column=0)

        self.freq1 = get_modes(resu1)
        self.freq2 = get_modes(resu2)
        # la variable NUMERIQUE qui contient ces memes listes. On remplit
        # ces valeurs quand on ferme la fenetre
        self.l1 = None
        self.l2 = None
        # la variable GRAPHIQUE qui donne le contenu des listes
        self.var_l1 = StringVar()
        self.var_l2 = StringVar()

        mac_win.set_modes(self.freq1, self.freq2, self.mac)

        # Une deuxieme fentre, dans laquelle on peut modifier l'appariement des
        # modes
        f = Frame(self.root)
        f.grid(row=1, column=0, sticky='news')
        f.columnconfigure(0, weight=1)
        f.columnconfigure(1, weight=4)

        Label(f, text="Liste de mode 1").grid(row=0, column=0, sticky='e')
        # l1 = Entry(f, textvariable=self.var_l1 )
        Label(f, textvariable=self.var_l1).grid(row=0, column=1, sticky='w')
        # l1.grid(row=0,column=1,sticky='ew')#,columnspan=3)
        Label(f, text="Liste de mode 2").grid(row=1, column=0, sticky='e')
        l2 = Entry(f, textvariable=self.var_l2)
        l2.grid(row=1, column=1, sticky='ew')  # ,columnspan=3)
        close = Button(f, text='Fermer', command=self.close_win)

        close.grid(row=2, column=1, sticky='e')

        self.set_default_pair()

        self.root.mainloop()

    def get_pair(self):
        """rend une double liste donnant les appariements de modes"""
        return [self.var_l1.get(), self.var_l2.get()]

    def set_pair(self, liste):
        """affiche une liste d'appariement donnee"""
        self.var_l1.set(liste[0])
        self.var_l2.set(liste[1])

    def set_default_pair(self):
        """ affiche la liste d'appariement par defaut. Le nombre de modes
            correspond au plus petit nombre de modes entre resu1 et resu2"""
        nb_mod = min(len(self.freq1), len(self.freq2))
        self.var_l1.set(range(1, nb_mod + 1))
        self.var_l2.set(range(1, nb_mod + 1))

    def close_win(self):
        self.l1 = self.var_l1.get()
        self.l2 = self.var_l2.get()
        self.root.quit()

    def get_list(self):
        exec('l1=' + self.l1)
        exec('l2=' + self.l2)
        return l1, l2
