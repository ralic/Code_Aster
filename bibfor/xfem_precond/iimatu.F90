function iimatu(i, ndim, nfh, nfe)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!-----------------------------------------------------------------------
! BUT : CALCULER L INDICE DANS LES MATRICES ELEMENTAIRES
!          POUR LES DDLS D ENRICHISSEMENT VECTORIEL
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
    integer ::i, ndim, nfh, nfe, iimatu
!-----------------------------------------------------------------------
!
     iimatu=i
     if (nfe.gt.0) then
        if (i.gt.ndim*(1+nfh)) then
          iimatu=int((i-ndim*(1+nfh)-1)/ndim)+ndim*(1+nfh)+1
        endif
     endif
!
end function
