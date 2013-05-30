subroutine rctype(jmat, nbpu, nompu, valpu, resu,&
                  type)
! -----------------------------------------------------------------
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
! ----------------------------------------------------------------------
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    integer :: imate, nbpu, jmat
    real(kind=8) :: valpu(*), resu
    character(len=*) :: nompu(*), type
! ----------------------------------------------------------------------
!     DETERMINATION DU TYPE DES VARIABLES DONT DEPEND LA COURBE DE
!     TRACTION
! IN  IMATE  : ADRESSE DU MATERIAU CODE
! IN  NBPU  : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
! IN  NOMPU : NOMS DES PARAMETRES "UTILISATEUR"
! IN  VALPU : VALEURS DES PARAMETRES "UTILISATEUR"
! OUT RESU  : VALEUR DU PARAMETRE DE LA FONCTION
! OUT TYPE  : TYPE DU PARAMETRE DE LA FONCTION
!
!
!
!
    integer :: icomp, ipi, idf, nbf, ivalk, ik, ipif, jpro
    integer :: nbmat
    character(len=16) :: nompf(2)
    character(len=24) :: valk
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
!-----------------------------------------------------------------------
    integer :: i, lfct, lmat, nbpara, nupar
!-----------------------------------------------------------------------
    parameter        ( lmat = 7 , lfct = 9 )
! DEB ------------------------------------------------------------------
!
    nbmat=zi(jmat)
!     UTILISABLE SEULEMENT AVEC UN MATERIAU PAR MAILLE
    call assert(nbmat.eq.1)
    imate = jmat+zi(jmat+nbmat+1)
!
    do 10 icomp = 1, zi(imate+1)
        if ('TRACTION' .eq. zk16(zi(imate)+icomp-1)(1:8)) then
            ipi = zi(imate+2+icomp-1)
            goto 11
        endif
10  end do
    call u2mess('F', 'ELEMENTS2_63')
11  continue
    idf = zi(ipi)+zi(ipi+1)
    nbf = zi(ipi+2)
    ivalk = zi(ipi+3)
    do 20 ik = 1, nbf
        if ('SIGM    ' .eq. zk8(ivalk+idf+ik-1)) then
            ipif = ipi+lmat-1+lfct*(ik-1)
            goto 21
        endif
20  end do
    call u2mess('F', 'MODELISA6_81')
21  continue
!
    jpro = zi(ipif+1)
!
    if (zk24(jpro) .eq. 'NAPPE') then
        nbpara = 2
        nompf(1) = zk24(jpro+2)
        nompf(2) = zk24(jpro+5)
        type = ' '
    else
        nbpara = 1
        if (zk24(jpro+2) .eq. 'EPSI') then
            resu = valpu(1)
            type = ' '
            goto 9999
        else
            valk = zk24(jpro)
            call u2mesg('F', 'MODELISA9_73', 1, valk, 0,&
                        0, 0, 0.d0)
        endif
    endif
!
    do 30 i = 1, nbpara
        if (nompf(i)(1:4) .ne. 'EPSI') then
            do 31 nupar = 1, nbpu
                if (nompu(nupar) .eq. nompf(i)) then
                    resu = valpu(nupar)
                    type = nompu(nupar)
                    goto 9999
                endif
31          continue
        endif
30  end do
!
    call u2mess('F', 'MODELISA9_83')
!
9999  continue
!
end subroutine
