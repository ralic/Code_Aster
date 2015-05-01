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

# Script permettant de recuperer les etudes ouvertes dans Salome

OUTPUTFILE1 = ''

try:
    import salome
    import SALOMEDS
    import salome_kernel
    orb, lcc, naming_service, cm = salome_kernel.salome_kernel_init()
    # get Study Manager reference
    obj = naming_service.Resolve('myStudyManager')
    myStudyManager = obj._narrow(SALOMEDS.StudyManager)
    List_Studies = myStudyManager.GetOpenStudies()

except Exception, e:
    List_Studies = []
    print "Probleme avec Salome, il n'est peut etre pas lance!"
    print e

try:
    if OUTPUTFILE1:
        # On ecrit les etudes ouvertes dans un fichier
        fw = file(OUTPUTFILE1, 'w')
        fw.write('\n'.join(List_Studies))

except Exception, e:
    raise Exception("Erreur : \n%s" % e)

print 62 * '-' + '\n' + '\n'.join(List_Studies) + '\n' + 62 * '-'
