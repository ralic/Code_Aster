subroutine ploint(vesto, modmec, chamno, num, i,&
                  vrai, model, veprj, modx, mody,&
                  modz)
!------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!------------------------------------------------------------------
    implicit none
!
! ROUTINE PROJETANT LA PRESSION ET LES DEPLACEMENTS SUR L'INTERFACE
!
! IN : MODMEC : NOM DU CONCEPT MODE_MECA
! IN : CHAMNO : CHAMNO DE DEPL_R
! IN : NUM : NUMEROTATION DES DDLS SUR MODELE INTERFACE
! IN: I : INDICE DE BOUCLES
! IN : VRAI : LOGICAL DISTINGUANT MODAL ET CHAMNO IMPOSE
! IN : MODEL : K2 :CHARACTER DISTINGUANT LE TYPE DE MODELE 2D OU 3D
! OUT :  VEPRJ : CHAMP DE PRESSION PROJETE
! OUT : MODX : CHAMP DES DEPLACEMENTS SUIVANT X
! OUT :  MODY : CHAMP DES DEPLACEMENTS SUIVANT Y
! OUT :  MODZ : CHAMP DES DEPLACEMENTS SUIVANT Z
!
!------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/chnucn.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
    logical(kind=1) :: vrai
    integer :: ipres, i, iret
    character(len=8) :: k8bid, tcorx(2), tcory(2), tcorz(2)
    character(len=*) :: modmec, chamno, model
    character(len=14) :: num
    character(len=19) :: vesto, modx, mody, modz, veprj
    character(len=24) :: nomcha, nocham
!-----------------------------------------------------------------
!
!-- PLONGEMENT DES VECTEURS PRESSIONS  POUR CHAQUE MODE SUR LE
!                          MODELE INTERFACE
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    modz=' '
    veprj = 'VEPRJ'
    call chnucn(vesto, num, 0, k8bid, 'V',&
                veprj)
    call jeveuo(veprj//'.VALE', 'L', ipres)
!
!
!---------------- PLONGEMENT DES MODES OU CHAMNO
!-------------PAR COMPOSANTES SUR LE MODELE INTERFACE
!
    if (vrai) then
!
        call rsexch(' ', modmec, 'DEPL', i, nomcha,&
                    iret)
        nocham = nomcha
!
    else
!
        nocham = chamno
!
    endif
!
!----- PLONGEMENT DE LA COMPOSANTE DX QUI DEVIENT TEMPERATURE
!
    tcorx(1) = 'DX'
    tcorx(2) = 'TEMP'
    modx = 'MODX'
    call chnucn(nocham, num, 2, tcorx, 'V',&
                modx)
!
!----- PLONGEMENT DE LA COMPOSANTE DY QUI DEVIENT TEMPERATURE
!
    tcory(1) = 'DY'
    tcory(2) = 'TEMP'
    mody = 'MODY'
    call chnucn(nocham, num, 2, tcory, 'V',&
                mody)
!
!----- PLONGEMENT DE LA COMPOSANTE DZ QUI DEVIENT TEMPERATURE
!
    if (model .eq. '3D') then
        tcorz(1) = 'DZ'
        tcorz(2) = 'TEMP'
        modz = 'MODZ'
        call chnucn(nocham, num, 2, tcorz, 'V',&
                    modz)
    endif
!
    call jedema()
end subroutine
