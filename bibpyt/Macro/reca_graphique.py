# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

import string
import copy
import Numeric
import types
import Cata
from Cata.cata import DEFI_FICHIER, IMPR_FONCTION
from Accas import _F

try:
    import Gnuplot
    isGnuplot = True
except:
    isGnuplot = False


#_____________________________________________
#
# IMPRESSIONS GRAPHIQUES
#_____________________________________________

def graphique(FORMAT, L_F, res_exp, reponses, iter, UL_out, interactif):

    if FORMAT == 'XMGRACE':
        for i in range(len(L_F)):
            _tmp = []
            courbe1 = res_exp[i]
            _tmp.append(
                {'ABSCISSE': courbe1[:, 0].tolist(), 'ORDONNEE': courbe1[:, 1].tolist(), 'COULEUR': 1})
            courbe2 = L_F[i]
            _tmp.append(
                {'ABSCISSE': courbe2[:, 0].tolist(), 'ORDONNEE': courbe2[:, 1].tolist(), 'COULEUR': 2})

            motscle2 = {'COURBE': _tmp}
            if interactif:
                motscle2['PILOTE'] = 'INTERACTIF'
            else:
                motscle2['PILOTE'] = 'POSTSCRIPT'

            IMPR_FONCTION(FORMAT='XMGRACE',
                          UNITE=int(UL_out),
                          TITRE='Courbe de : ' + reponses[i][0],
                          SOUS_TITRE='Iteration : ' + str(iter),
                          LEGENDE_X=reponses[i][1],
                          LEGENDE_Y=reponses[i][2],
                          **motscle2
                          )

    elif FORMAT == 'GNUPLOT':
        if isGnuplot:
            graphe = []
            impr = Gnuplot.Gnuplot()
            Gnuplot.GnuplotOpts.prefer_inline_data = 1
            # impr('set data style linespoints')
            impr('set grid')
            impr('set pointsize 2.')
            impr('set terminal postscript color')
            impr('set output "fort.' + str(UL_out) + '"')

            for i in range(len(L_F)):
                if interactif:
                    graphe.append(Gnuplot.Gnuplot(persist=0))
                    # graphe[i]('set data style linespoints')
                    graphe[i]('set grid')
                    graphe[i]('set pointsize 2.')
                    graphe[i].xlabel(reponses[i][1])
                    graphe[i].ylabel(reponses[i][2])
                    graphe[i].title(
                        reponses[i][0] + '  Iteration ' + str(iter))
                    graphe[i].plot(
                        Gnuplot.Data(L_F[i], title='Calcul'), Gnuplot.Data(res_exp[i], title='Experimental'))
                    graphe[i]('pause 5')

                impr.xlabel(reponses[i][1])
                impr.ylabel(reponses[i][2])
                impr.title(reponses[i][0] + '  Iteration ' + str(iter))
                impr.plot(Gnuplot.Data(L_F[i], title='Calcul'), Gnuplot.Data(
                    res_exp[i], title='Experimental'))

    else:
        pass
