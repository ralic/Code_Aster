function iposdg(dg, cmp)
    implicit none
    integer :: iposdg
!
    integer :: dg(*), cmp
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_6
!
!***********************************************************************
!
!     REND LA POSITION D'1 CMP DANS UN DESCRIPTEUR-GRANDEUR DG
!
!       DG   (IN) : TABLE DES ENTIERS CODES
!
!       CMP  (IN) : NUMERO DE LA COMPOSANTE
!
!     EXEMPLE
!
!      POUR LE GRANDEUR DEPLA_R :
!
!         L' ENVELOPPE DU DESCRIPTEUR EST
!
!            DX DY DZ DRX DRY DRZ LAGR
!
!         SUPPOSONS QUE LA DESCRIPTION LOCALE SOIT : DX DZ DRY
!
!         ALORS IPOSDG(DG,NUM('DZ') ---> 2
!
!         LA COMPOSAMTE DZ APPARAIT EN POSITION 2 DANS LA DESCRIPTION
!
!***********************************************************************
!
    integer :: paquet, valec, nbec, reste, code, cmpt, i, lshift
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nbec = (cmp - 1)/30 + 1
    cmpt = 0
    reste = cmp - 30*(nbec-1)
    code = lshift(1,reste)
!
    if (iand(dg(nbec),code) .eq. code) then
!
        do 10, paquet = 1, nbec-1, 1
!
        valec = dg(paquet)
!
        do 11, i = 1, 30, 1
!
        code = lshift(1,i)
!
        if (iand(valec,code) .eq. code) then
!
            cmpt = cmpt + 1
!
        endif
!
11      continue
!
10      continue
!
        valec = dg(nbec)
!
        do 20, i = 1, reste, 1
!
        code = lshift(1,i)
!
        if (iand(valec,code) .eq. code) then
!
            cmpt = cmpt + 1
!
        endif
!
20      continue
!
    endif
!
    iposdg = cmpt
!
end function
