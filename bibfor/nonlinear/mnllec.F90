subroutine mnllec(imat, numedd, ordman, epsman, pasman,&
                  epscor, h, hf, itemax, nbran,&
                  nextr, epsbif)
    implicit none
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
! ----------------------------------------------------------------------
!
!       MODE NON LINEAIRE - LECTURE DES DONNEES
!       -         -         ---
! ----------------------------------------------------------------------
!
! LIT LES DONNEES DU FICHIER DE COMMANDE
! ----------------------------------------------------------------------
!
! OUT  IMAT   : I(2) : DESCRIPTEUR DES MATRICES :
!                       - IMAT(1) => MATRICE DE RAIDEUR
!                       - IMAT(2) => MATRICE DE MASSE
! OUT  NUMEDD : K24  : NUME_DDL DES MATRICES DE MASSE ET RAIDEUR
! IN   TFREQ  : K16  : TYPE CALCUL DE FREQUENCE
! IN   NMNL   : I    : SI TFREQ='PLUS_PETITE'
!                         ALORS NMNL=NOMBRE DE MODES NON-LINEAIRES
! OUT  BANDE  : R8(2): SI TFREQ='BANDE'
!                         ALORS BANDE=(FREQ1,FREQ2)
! OUT  ORDMAN : I    : ORDRE DE LA MAN
! OUT  EPSMAN : R8   : PRECISION DE LA MAN
! OUT  PASMAN : I    : PAS DE LA MAN
! OUT  EPSCOR : R8   : PRECISION DE LA CORRECTION
! OUT  H      : I    : NOMBRE D'HARMONIQUES POUR X
! OUT  HF     : I    : NOMBRE D'HARMONIQUES POUR F
! OUT  ITEMAX : I    : NOMBRE D ITERATIONS MAX POUR LA CORRECTION
! OUT  NBRAN  : I    : NOMBRE DE BRANCHES A CALCULER
! OUT  NEXTR  : I    : NOMBRE DE TERME A PRENDRE EN COMPTE POUR LA DETECTION DE BIFURCATION
! OUT  EPSBIF : R8   : RESIDU POUR LA BIFURCATION
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdscr.h"
#include "asterfort/utmess.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
!
    integer :: imat(2), ordman, pasman, h, hf, itemax, nbran, nextr
    real(kind=8) :: epsman, epscor, epsbif
    character(len=24) :: numedd
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    integer :: imeth
    character(len=8) :: masse, rigid
    character(len=16) :: method
!
! ----------------------------------------------------------------------
! --- RECUPERATION DES MATRICES ET DU NUME_DDL
! ----------------------------------------------------------------------
    call getvid(' ', 'MATR_RIGI', scal=rigid)
    call getvid(' ', 'MATR_MASS', scal=masse)
    call mtdscr(rigid)
    call jeveuo(rigid//'           .&INT', 'E', imat(1))
    call mtdscr(masse)
    call jeveuo(masse//'           .&INT', 'E', imat(2))
    call dismoi('NOM_NUME_DDL', rigid, 'MATR_ASSE', repk=numedd)
! ----------------------------------------------------------------------
! --- ON RECUPERE LE NBRE D'HARMONIQUE (DEPL ET FORCE)
! ----------------------------------------------------------------------
    call getvis('RESOLUTION', 'NB_HARM_LINE', iocc=1, scal=h)
    call getvis('RESOLUTION', 'NB_HARM_NONL', iocc=1, scal=hf)
    if (h .gt. hf) then
        call utmess('F', 'MECANONLINE9_63')
    endif
! ----------------------------------------------------------------------
! --- CHOIX DE LA METHODE DE CONTINUATION
! ----------------------------------------------------------------------
    call getvtx('RESOLUTION', 'METHODE', iocc=1, scal=method)
    if (method .eq. 'EHMAN') then
        imeth = 1
    endif
! ----------------------------------------------------------------------
! --- PARAMETRE DE LA MAN
! ----------------------------------------------------------------------
    if (imeth .eq. 1) then
! ----- ORDRE DE LA MAN
        call getvis('RESOLUTION', 'NB_ORDRE_MAN', iocc=1, scal=ordman)
! ----- PRECISION DE LA MAN
        call getvr8('RESOLUTION', 'PREC_MAN', iocc=1, scal=epsman)
! ----- PRECISION DE LA CORRECTION DE NEWTON
        call getvr8('RESOLUTION', 'PREC_NEWTON', iocc=1, scal=epscor)
! ----- NOMBRE D'ITERATIONS MAXIMALES DE LA CORRECTION DE NEWTON
        call getvis('RESOLUTION', 'ITER_NEWTON_MAXI', iocc=1, scal=itemax)
! ----- NOMBRE DE PAS DE LA MAN
        call getvis('RESOLUTION', 'NB_PAS_MAN', iocc=1, scal=pasman)
        pasman=pasman+1
! ----- NOMBRE DE BRANCHE
        call getvis('RESOLUTION', 'NB_BRANCHE', iocc=1, scal=nbran)
! ----- NOMBRE DE TERME A PRENDRE EN COMPTE POUR LA DETECTION DE BIFURCATION
        call getvis('RESOLUTION', 'CRIT_ORDR_BIFURCATION', iocc=1, scal=nextr)
! ----- RESIDU BIFURCATION
        call getvr8('RESOLUTION', 'RESI_RELA_BIFURCATION', iocc=1, scal=epsbif)
    endif
end subroutine
