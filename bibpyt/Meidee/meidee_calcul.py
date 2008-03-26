#@ MODIF meidee_calcul Meidee  DATE 26/03/2008   AUTEUR BODEL C.BODEL 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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


## \package meidee_calcul outils de calcul pour meidee
#
# Ce module contient l'ensemble des classes de calcul de meidee

############# IMPORTANT ########################################
# Ce module est utilise en non-interactif et ne doit donc pas  #
# importer Tkinter directement ou indirectement                #
################################################################


class MessageBox:
    """!Classe qui permet d'ecrire dans un .mess separe"""
    def __init__(self, unite):
        self.unite = unite #unite d'ecriture
        self.mess_file = open('fort.'+str(unite),'w')

    def disp_mess(self, new_mess):
        """!Ecriture des messages dans le fichier sortie
        s'il existe et dans la fenetre de message"""
        self.mess_file.writelines(new_mess + '\n')

    def close_file(self):
        """ Ferme le fichier de message a la fin de l'utilisation"""
        self.mess_file.close()
