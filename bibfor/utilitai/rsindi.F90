subroutine rsindi(tysca, iaobj, paobj, jordr, ival,&
                  rval, kval, cval, epsi, crit,&
                  nbordr, nbtrou, nutrou, ndim)
    implicit none
    include 'jeveux.h'
    include 'asterfort/u2mesk.h'
    integer :: nbordr, nbtrou, nutrou(*), ndim, ival, paobj
    real(kind=8) :: rval, epsi
    character(len=4) :: tysca
    character(len=*) :: kval, crit
    complex(kind=8) :: cval
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!      TROUVER DANS LA TABLE ZX(IAOBJ-1+I),I=1,NBORDR LE SCALAIRE
!      IVAL,RVAL,CVAL...
!               AVEC LA PRECISION  / RELATIVE EPSI
!                                  / ABSOLUE  EPSI
!      LE TEST FAIT EST : ABS(V-VR).LE.ABS(EPSI*VR) EN RELATIF
!                    OU   ABS(V-VR).LE.ABS(EPSI)    EN ABSOLU
! ----------------------------------------------------------------------
! IN  : TYSCA  : R8 OU C16 OU I8 OU K8,  K16, K24, K32, K80
! IN  : IAOBJ  : ADRESSE DE LA TABLE DANS ZI, ZR OU ZC
! IN  : PAOBJ  : "PAS" dU parcours de l'objet :
!                 ZX(IAOBJ),ZX(IAOBJ+1*PAOBJ),,ZX(IAOBJ+2*PAOBJ), ...
! IN  : JORDR  : ADRESSE DU .ORDR DU RESULTAT
! IN  : IVAL   : VALEUR CHECHEE SI ENTIERE.
! IN  : RVAL   : VALEUR CHECHEE SI REELLE.
! IN  : KVAL   : VALEUR CHECHEE SI CARACTERE.
! IN  : CVAL   : VALEUR CHECHEE SI COMPLEXE.
! IN  : CRIT   : CRITERE : 'RELATIF' OU 'ABSOLU'
! IN  : EPSI   : PRECISION VOULUE.
! IN  : NBORDR : DIMENSION MAXI DE LA TABLE DE RECHERCHE.
! IN  : NDIM   : DIMENSION MAXI DE LA LISTE A REMPLIR.
! OUT : NBTROU : NOMBRE DE VALEURS CONVENABLES.
! OUT : NUTROU : LISTE DES INDICES DES VALEURS CONVENABLES.
!                   SI NBTROU EST > NDIM , ON SIGNALE L'ERREUR EN
!                   RENDANT NBTROU = - NBTROU
! ----------------------------------------------------------------------
    character(len=8) :: crit2
    logical :: depass, trouve
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iaobj, jordr
!-----------------------------------------------------------------------
    crit2 = crit
    nbtrou = 0
    depass = .false.
    if (tysca(1:1) .eq. 'R') then
        do 10 i = 1, nbordr
            if (crit2(1:4) .eq. 'RELA') then
                if (abs(zr(iaobj+(i-1)*paobj)-rval) .le. abs(epsi*rval)) then
                    trouve = .true.
                else
                    trouve = .false.
                endif
            else if (crit2(1:4).eq.'ABSO') then
                if (abs(zr(iaobj+(i-1)*paobj)-rval) .le. abs(epsi)) then
                    trouve = .true.
                else
                    trouve = .false.
                endif
            else
                call u2mesk('F', 'ALGORITH3_42', 1, crit2)
            endif
            if (trouve) then
                nbtrou = nbtrou + 1
                if (nbtrou .le. ndim) then
                    nutrou(nbtrou) = zi(jordr+i-1)
                else
                    depass = .true.
                endif
            endif
10      continue
    else if (tysca(1:1).eq.'I') then
        do 20 i = 1, nbordr
            if (zi(iaobj+(i-1)*paobj) .eq. ival) then
                nbtrou = nbtrou + 1
                if (nbtrou .le. ndim) then
                    nutrou(nbtrou) = zi(jordr+i-1)
                else
                    depass = .true.
                endif
            endif
20      continue
    else if (tysca.eq.'K8  ') then
        do 30 i = 1, nbordr
            if (zk8(iaobj+(i-1)*paobj) .eq. kval) then
                nbtrou = nbtrou + 1
                if (nbtrou .le. ndim) then
                    nutrou(nbtrou) = zi(jordr+i-1)
                else
                    depass = .true.
                endif
            endif
30      continue
    else if (tysca.eq.'K16 ') then
        do 40 i = 1, nbordr
            if (zk16(iaobj+(i-1)*paobj) .eq. kval) then
                nbtrou = nbtrou + 1
                if (nbtrou .le. ndim) then
                    nutrou(nbtrou) = zi(jordr+i-1)
                else
                    depass = .true.
                endif
            endif
40      continue
    else if (tysca.eq.'K24 ') then
        do 50 i = 1, nbordr
            if (zk24(iaobj+(i-1)*paobj) .eq. kval) then
                nbtrou = nbtrou + 1
                if (nbtrou .le. ndim) then
                    nutrou(nbtrou) = zi(jordr+i-1)
                else
                    depass = .true.
                endif
            endif
50      continue
    else if (tysca.eq.'K32 ') then
        do 60 i = 1, nbordr
            if (zk32(iaobj+(i-1)*paobj) .eq. kval) then
                nbtrou = nbtrou + 1
                if (nbtrou .le. ndim) then
                    nutrou(nbtrou) = zi(jordr+i-1)
                else
                    depass = .true.
                endif
            endif
60      continue
    else if (tysca.eq.'K80 ') then
        do 70 i = 1, nbordr
            if (zk80(iaobj+(i-1)*paobj) .eq. kval) then
                nbtrou = nbtrou + 1
                if (nbtrou .le. ndim) then
                    nutrou(nbtrou) = zi(jordr+i-1)
                else
                    depass = .true.
                endif
            endif
70      continue
    else if (tysca(1:1).eq.'C') then
        do 80 i = 1, nbordr
            if (crit2(1:4) .eq. 'RELA') then
                if (abs(zc(iaobj+(i-1)*paobj)-cval) .le. abs(epsi*cval)) then
                    trouve = .true.
                else
                    trouve = .false.
                endif
            else if (crit2(1:4).eq.'ABSO') then
                if (abs(zc(iaobj+(i-1)*paobj)-cval) .le. abs(epsi)) then
                    trouve = .true.
                else
                    trouve = .false.
                endif
            else
                call u2mesk('F', 'ALGORITH3_42', 1, crit2)
            endif
            if (trouve) then
                nbtrou = nbtrou + 1
                if (nbtrou .le. ndim) then
                    nutrou(nbtrou) = zi(jordr+i-1)
                else
                    depass = .true.
                endif
            endif
80      continue
    else
        call u2mesk('F', 'UTILITAI4_33', 1, tysca)
    endif
!
!
    if (depass) nbtrou = -nbtrou
!
end subroutine
