subroutine cfatmu(neq, nesmax, ndim, nbliac, frot,&
                  llf, llf1, llf2, resoco)
! ======================================================================
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/calatm.h'
    include 'asterfort/cftyli.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: neq
    integer :: nesmax
    integer :: ndim
    integer :: nbliac
    integer :: llf
    integer :: llf1
    integer :: llf2
    integer :: frot
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! CALCUL DE ATMU - VECTEUR DES FORCES DE CONTACT
!
!
! ----------------------------------------------------------------------
!
!
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
!              (SERT A DECALER LES POINTEURS POUR LE FROTTEMENT 3D)
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!                'E': RESOCO(1:14)//'.ATMU'
! IN  LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! IN  LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! IN  LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
! IN  FROT   : VAUT 1 LORSQU'IL Y A DU FROTTEMENT, 0 SINON
!
!
!
!
!
    integer :: btotal, iliac, lliac, jdecal
    integer :: posit, nbddl
    integer :: deklag, compts, compt0, compt1, compt2, kk
    character(len=19) :: liac, mu, atmu
    integer :: jliac, jmu, jatmu
    character(len=24) :: appoin, apddl, apcoef, apcofr
    integer :: japptr, japddl, japcoe, japcof
! ======================================================================
    call jemarq()
! ======================================================================
! --- APPEL JEVEUX POUR LA MISE A JOUR DES VECTEURS DE LIAISONS --------
! ======================================================================
    liac = resoco(1:14)//'.LIAC'
    mu = resoco(1:14)//'.MU'
    atmu = resoco(1:14)//'.ATMU'
    appoin = resoco(1:14)//'.APPOIN'
    apddl = resoco(1:14)//'.APDDL'
    apcoef = resoco(1:14)//'.APCOEF'
    apcofr = resoco(1:14)//'.APCOFR'
    call jeveuo(liac, 'L', jliac)
    call jeveuo(mu, 'L', jmu)
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(atmu, 'E', jatmu)
!
!
    if (frot .eq. 1) then
        call jeveuo(apcofr, 'L', japcof)
    endif
!
! --- INITIALISATION
!
    do 170 kk = 1, neq
        zr(jatmu+kk-1) = 0.0d0
170  end do
!
! ======================================================================
! --- CALCUL DE AT.MU --------------------------------------------------
! ======================================================================
    btotal = nbliac + llf + llf1 + llf2
    deklag = 0
    compts = 0
    compt0 = nbliac
    compt1 = nbliac + (ndim-1)*llf
    compt2 = nbliac + (ndim-1)*llf + llf1
    do 10 iliac = 1, btotal
        lliac = zi(jliac +iliac-1)
        jdecal = zi(japptr+lliac-1)
        nbddl = zi(japptr+lliac ) - zi(japptr+lliac-1)
        call cftyli(resoco, iliac, posit)
        goto (1000, 2000, 3000, 4000) posit
1000      continue
! ======================================================================
! --- CAS D'UNE LIAISON DE CONTACT -------------------------------------
! ======================================================================
        compts = compts + 1
        call calatm(neq, nbddl, zr(jmu-1+compts), zr(japcoe+jdecal), zi( japddl+jdecal),&
                    zr(jatmu))
        goto 10
2000      continue
! ======================================================================
! --- CAS D'UNE LIAISON DE FROTTEMENT ADHERENT SUIVANT LES DEUX --------
! --- DIRECTION OU DANS LE CAS GENERAL EN 2D ---------------------------
! ======================================================================
        compt0 = compt0 + 1
        call calatm(neq, nbddl, zr(jmu-1+compt0), zr(japcof+jdecal), zi( japddl+jdecal),&
                    zr(jatmu))
        if (ndim .eq. 3) then
            deklag = deklag + 1
            call calatm(neq, nbddl, zr(jmu-1+compt0+llf), zr(japcof+ jdecal+30*nesmax),&
                        zi(japddl+jdecal), zr(jatmu))
        endif
        if (frot .eq. 0) then
            call assert(.false.)
        endif
        goto 10
3000      continue
! ======================================================================
! --- CAS D'UNE LIAISON DE FROTTEMENT ADHERENT SUIVANT LA PREMIERE -----
! --- DIRECTION --------------------------------------------------------
! ======================================================================
        compt1 = compt1 + 1
        call calatm(neq, nbddl, zr(jmu-1+compt1), zr(japcof+jdecal), zi( japddl+jdecal),&
                    zr(jatmu))
        if (frot .eq. 0) then
            call assert(.false.)
        endif
        goto 10
4000      continue
! ======================================================================
! --- CAS D'UNE LIAISON DE FROTTEMENT ADHERENT SUIVANT LA SECONDE ------
! --- DIRECTION --------------------------------------------------------
! ======================================================================
        compt2 = compt2 + 1
        call calatm(neq, nbddl, zr(jmu-1+compt2), zr(japcof+jdecal+30* nesmax),&
                    zi(japddl+jdecal), zr(jatmu))
        if (frot .eq. 0) then
            call assert(.false.)
        endif
10  end do
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
