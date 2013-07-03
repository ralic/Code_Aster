subroutine mat152(option, model, moint, nocham, ivalk,&
                  nbmo, max, may, maz, num)
    implicit none
!---------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!---------------------------------------------------------------------
! AUTEUR : G.ROUSSEAU
!
! ROUTINE PERMETTANT LE CALCUL DE MATRICE ASSEMBLEES INTERVENANT
! DANS LE CALCUL DES COEFFICIENTS AJOUTES :
! MAX, MAY, MAZ : MATRICES INDEPENDANTES DU MODE POUR
! CALCULER LA MASSE AJOUTEE
! BI : MATRICES MODALES INTERVENANT DANS LE CALCUL DE L AMORTISSEMENT
!      ET DE LA RAIDEUR
!
! IN : K* : MODEL : DIMENSION DU MODELE 2D,3D OU AXI
! IN : K* : MOINT : MODELE D INTERFACE
! IN : K* : NOCHAM : NOM DU CHAMP AUX NOEUDS DE DEPL_R
! IN : I  : IVALK : ADRESSE DU TABLEAU DES NOMS DES CHAMNOS
! IN : I  : NBMO  : NOMBRE DE MODES OU DE CHAMNOS UTILISATEURS
! OUT : K19 : MAX,MAY,MAZ : NOMS DES MATRICES POUR LE CALCUL DE MASSE
! OUT : K14 : NUM : NUMEROTATION DES DDLS THERMIQUES D 'INTERFACE
!---------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getvid.h"
#include "asterfort/ca2mam.h"
#include "asterfort/calmaa.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/megeom.h"
#include "asterfort/rsexch.h"
#include "asterfort/wkvect.h"
    integer :: nbmo, imode, imade, iret
    integer :: ivalk
    integer :: n5, n6, n7
    character(len=1) :: dir
    character(len=2) :: model
    character(len=3) :: incr
    character(len=8) :: modmec
    character(len=8) :: moint
    character(len=8) :: lpain(2), lpaout(1)
    character(len=9) :: option
    character(len=14) :: num
    character(len=19) :: max, may, maz, chamno
    character(len=24) :: chgeom, lchin(2)
    character(len=24) :: nomcha, nocham
    character(len=24) :: ligrmo, made
    integer :: iarg
! -----------------------------------------------------------------
!--------------------------------------------------------------
! CALCUL DES MATR_ELEM AX ET AY DANS L'OPTION FLUX_FLUI_X ET _Y
!---------------SUR LE MODELE INTERFACE(THERMIQUE)-------------
!
    call jemarq()
    n7=0
    if (getexm(' ','CHAM_NO') .eq. 1) then
        call getvid(' ', 'CHAM_NO', 0, iarg, 0,&
                    chamno, n6)
        n7 =-n6
    endif
    call getvid(' ', 'MODE_MECA', 0, iarg, 1,&
                modmec, n5)
!------ RECUPERATION D'ARGUMENTS COMMUNS AUX CALCULS DES DEUX ---
!-------------------------MATR_ELEM -----------------------------
!
    ligrmo = moint(1:8)//'.MODELE'
    call megeom(moint(1:8), chgeom)
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PACCELR'
    lpaout(1) = 'PMATTTR'
    maz=' '
!----------------------------------------------------------------
!-----------CALCUL DE LA MATRICE AZ DES N(I)*N(J)*NZ-------------
!----------------------------------------------------------------
    if (model .eq. '3D') then
        dir='Z'
        call calmaa(moint, ' ', dir, ligrmo, lchin(1),&
                    lpain(1), lpaout(1), num, maz)
!
!
    endif
!----------------------------------------------------------------
!-----------CALCUL DE LA MATRICE AX DES N(I)*N(J)*NX-------------
!----------------------------------------------------------------
!
    dir='X'
    call calmaa(moint, ' ', dir, ligrmo, lchin(1),&
                lpain(1), lpaout(1), num, max)
!
!
!----------------------------------------------------------------
!-----------CALCUL DE LA MATRICE AY DES N(I)*N(J)*NY-------------
!----------------------------------------------------------------
!
    dir='Y'
    call calmaa(moint, ' ', dir, ligrmo, lchin(1),&
                lpain(1), lpaout(1), num, may)
!
!----------------------------------------------------------------
!----------CALCUL DE LA MATRICE MODALE DES DN(I)*DN(J)*
!-----------------------MODE*NORMALE-----------------------------
!----------------------------------------------------------------
    if (option .eq. 'AMOR_AJOU' .or. option .eq. 'RIGI_AJOU') then
!
        call wkvect('&&MAT152.MADE', 'V V K24', nbmo, imade)
!
        do 6000 imode = 1, nbmo
            incr='BID'
            if (n7 .gt. 0) then
                lchin(2)=zk8(ivalk+imode-1)
            else
                call rsexch(' ', modmec, 'DEPL', imode, nomcha,&
                            iret)
                lchin(2) =nomcha
            endif
            call codent(imode, 'D0', incr(1:3))
            call ca2mam(moint, incr, ligrmo, lchin, lpain,&
                        lpaout, num, made)
            zk24(imade+imode-1)= made
6000      continue
!
    endif
    call jedema()
end subroutine
