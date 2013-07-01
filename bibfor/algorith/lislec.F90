subroutine lislec(motfac, phenoz, base, lischa)
!
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
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/liscrs.h"
#include "asterfort/lisdef.h"
#include "asterfort/lislef.h"
#include "asterfort/lisnnl.h"
#include "asterfort/lisnnn.h"
#include "asterfort/lissav.h"
#include "asterfort/listap.h"
    character(len=*) :: phenoz
    character(len=16) :: motfac
    character(len=19) :: lischa
    character(len=1) :: base
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! LECTURE DES CHARGEMENTS
!
! ----------------------------------------------------------------------
!
!
! IN  PHENOM : TYPE DE PHENOMENE (MECANIQUE, THERMIQUE, ACOUSTIQUE)
! IN  MOTFAC : MOT-CLEF FACTEUR DES EXCITATIONS
! IN  BASE   : BASE DE CREATION DE LA SD LISCHA
! OUT LISCHA : SD LISTE DES CHARGES
!
! ----------------------------------------------------------------------
!
    integer :: iexci, nbexci, ibid
    character(len=8) :: charge, k8bid
    character(len=16) :: typapp, typfct
    integer :: genrec, motclc(2)
    character(len=8) :: typech, nomfct
    character(len=13) :: prefob
    real(kind=8) :: phase
    integer :: npuis
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOMBRE D'EXCITATIONS
!
    call getfac(motfac, nbexci)
!
! --- CREATION SD CHARGEMENT
!
    if (nbexci.ne.0) call liscrs(lischa, nbexci, base)
!
! --- LECTURE OCCURRENCES EXCIT
!
    do 100 iexci = 1, nbexci
!
! ----- LECTURE NOM DE LA CHARGE (PROVENANT DE AFFE_CHAR_*)
!
        call lisnnn(motfac, iexci, charge)
!
! ----- PREFIXE DE L'OBJET DE LA CHARGE
!
        call lisnnl(phenoz, charge, prefob)
!
! ----- GENRES DE LA CHARGE
!
        call lisdef('IDGE', prefob, ibid, k8bid, genrec)
!
! ----- MOT-CLEFS DE LA CHARGE
!
        call lisdef('IDMC', prefob, ibid, k8bid, motclc)
!
! ----- TYPE DE LA CHARGE (COMPLEXE, FONCTION, REELLE)
!
        call lisdef('TYPC', prefob, genrec, typech, ibid)
!
! ----- TYPE D'APPLICATION DE LA CHARGE
!
        call listap(motfac, iexci, typapp)
!
! ----- RECUPERATION FONCTION MULTIPLICATRICE
!
        call lislef(motfac, iexci, nomfct, typfct, phase,&
                    npuis)
!
! ----- SAUVEGARDE DES INFORMATIONS
!
        call lissav(lischa, iexci, charge, typech, genrec,&
                    motclc, prefob, typapp, nomfct, typfct,&
                    phase, npuis)
!
100 continue
!
    call jedema()
end subroutine
