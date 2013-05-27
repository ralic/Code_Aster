subroutine lischk(nomo, phenoz, nomcmz, lischa)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lisccm.h'
    include 'asterfort/lisccp.h'
    include 'asterfort/liscom.h'
    include 'asterfort/lisdbl.h'
    include 'asterfort/lisver.h'
    character(len=*) :: nomcmz, phenoz
    character(len=8) :: nomo
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! VERIFICATION DE LA LISTE DES CHARGES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! IN  PHENOM : TYPE DE PHENOMENE (MECANIQUE, THERMIQUE, ACOUSTIQUE)
! IN  NOMCMD : NOM DE LA COMMANDE
! IN  LISCHA : SD LISTE DES CHARGES
!
!
!
!
    character(len=16) :: nomcmd, phenom
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomcmd = nomcmz
    phenom = phenoz
!
! --- VERIFICATION DE LA COHERENCE DES MODELES
!
    call liscom(nomo, lischa)
!
! --- VERIFICATION COMPATIBILITE CHARGE/PHENOMENE
!
    call lisccp(phenom, lischa)
!
! --- VERIFICATION COMPATIBILITE CHARGE/COMMANDE
!
    call lisccm(nomcmd, lischa)
!
! --- VERIFICATIONS DES DOUBLONS
!
    call lisdbl(lischa)
!
! --- VERIFICATIONS DIVERSES SUR LES TYPES DE CHARGES
!
    call lisver(lischa)
!
    call jedema()
end subroutine
