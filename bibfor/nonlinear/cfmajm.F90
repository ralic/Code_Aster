subroutine cfmajm(resoco, ndim, nbliac, llf, llf1,&
                  llf2)
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
    include 'asterfort/cftyli.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    integer :: ndim, nbliac, llf, llf1, llf2
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! RE-ORDONNE LE VECTEUR MU
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! IN  LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! IN  LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
!
!
!
!
!
    integer :: jmu, iliac, posit, posmu, jmajmu, dimmaj
    integer :: posnbl, poslf0, poslf1, poslf2
    character(len=19) :: mu
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    posmu = 0
    posnbl = 0
    poslf0 = nbliac
    poslf1 = nbliac + (ndim-1)*llf
    poslf2 = nbliac + (ndim-1)*llf + llf1
    dimmaj = nbliac + (ndim-1)*llf + llf1 + llf2
!
! --- ACCES STRUCTURES DE DONNEES DE CONTACT
!
    mu = resoco(1:14)//'.MU'
    call jeveuo(mu, 'E', jmu)
!
! --- RECOPIE DES VALEURS DANS UN VECTEUR TEMPORAITE
!
    call wkvect('&&CFMAJM.ORDO', 'V V R', dimmaj, jmajmu)
    do 20 iliac = 1, dimmaj
        zr(jmajmu-1+iliac) = zr(jmu-1+iliac)
20  end do
!
! --- ECRIT LES LIAISONS DANS L'ORDRE: NBLIAC, LLF, LLF1, LLF2
!
    do 10 iliac = 1, nbliac + llf + llf1 + llf2
        posmu = posmu + 1
        call cftyli(resoco, iliac, posit)
        goto (1000,2000,3000,4000) posit
!
! ----- LIAISON DE CONTACT
!
1000      continue
        posnbl = posnbl + 1
        zr(jmu-1+posnbl) = zr(jmajmu-1+posmu)
        goto 10
!
! ----- LIAISON DE FROTTEMENT - 2D OU 3D DANS LES DEUX DIRECTIONS
!
2000      continue
        poslf0 = poslf0 + 1
        zr(jmu-1+poslf0) = zr(jmajmu-1+posmu)
        if (ndim .eq. 3) then
            posmu = posmu + 1
            zr(jmu-1+poslf0+llf) = zr(jmajmu-1+posmu)
        endif
        goto 10
!
! ----- LIAISON DE FROTTEMENT - 3D PREMIERE DIRECTION
!
3000      continue
        poslf1 = poslf1 + 1
        zr(jmu-1+poslf1) = zr(jmajmu-1+posmu)
        goto 10
!
! ----- LIAISON DE FROTTEMENT - 3D SECONDE DIRECTION
!
4000      continue
        poslf2 = poslf2 + 1
        zr(jmu-1+poslf2) = zr(jmajmu-1+posmu)
10  end do
!
    call jedetr('&&CFMAJM.ORDO')
    call jedema()
!
end subroutine
