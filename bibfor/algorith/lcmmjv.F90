subroutine lcmmjv(comp, nmat, cpmono, nbfsys, irota,&
                  itbint, nfs, nsg, hsr)
    implicit none
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
! person_in_charge: jean-michel.proix at edf.fr
!     ----------------------------------------------------------------
!     MONOCRISTAL : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
!     ----------------------------------------------------------------
!     IN  COMP   : OBJET COMPOR
!         NMAT   :  DIMENSION  MAXIMUM DE CPMONO
!     OUT CPMONO : NOMS DES LOIS POUR CHAQUE FAMILLE DE SYSTEME
!         NBFSYS : NOMBRE DE FAMILLES DE SYS GLIS
!         IROTA  : 1 POUR ROTATION DE RESEAU
!         ITBINT : 1 SI MATRICE D'INTERACTION DONNEE PAR L'UTILISATEUR
!         HSR    : MATRICE D'INTERACTION POUR L'ECROUISSAGE ISOTROPE
!                  UTILISEE SEULEMENT POUR LE MONOCRISTAL IMPLICITE
!     COMMON
!         TBSYSG : SYSTEMES DE GLISSEMENT DONNES PAR L'UTILISATEUR
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/r8inir.h'
    include 'blas/dcopy.h'
    integer :: nmat, icompi, irota, itbint, icompo, nbfsys, i, j, nfs, nsg
    integer :: icompr, nbsyst, nbtbsy, ifa, nbsysi, idecal
    real(kind=8) :: hsr(nsg, nsg), tbsysg
    character(len=16) :: comp(*), compk, compi, compr
    character(len=24) :: cpmono(5*nmat+1)
    common/tbsysg/tbsysg(900)
!     ----------------------------------------------------------------
!
! -   NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
!
!
    call jemarq()
!
    compk=comp(7)(1:8)//'.CPRK'
    compi=comp(7)(1:8)//'.CPRI'
    compr=comp(7)(1:8)//'.CPRR'
    call jeveuo(compk, 'L', icompo)
    call jeveuo(compi, 'L', icompi)
!
    nbfsys=zi(icompi-1+5)
    irota =zi(icompi-1+6)
    itbint=zi(icompi-1+4)
    nbsyst=zi(icompi-1+8)
!
!     5 FAMILLES DE SYSTEMES MAXI
    do 1 i = 1, 5*nbfsys
        cpmono(i)=zk24(icompo-1+i)
 1  end do
!
    cpmono(5*nbfsys+1)=zk24(icompo-1+5*nbfsys+1)
!
    nbtbsy=0
    do 3 ifa = 1, nbfsys
        nbsysi=zi(icompi-1+8+ifa)
        nbtbsy=nbtbsy+nbsysi
 3  end do
!
    if (nbtbsy .ne. 0) then
        call r8inir(900, 0.d0, tbsysg, 1)
        call jeveuo(compr, 'L', icompr)
!           TABLE CONTENANT LES SYSTEMES
        call dcopy(6*nbtbsy+12, zr(icompr), 1, tbsysg, 1)
    else
        tbsysg(1)=0.d0
    endif
!     TABLE CONTENANT LA MATRICE D'INTERACTION
    if (itbint .eq. 1) then
        idecal=0
        if (nbtbsy .eq. 0) then
            call jeveuo(compr, 'L', icompr)
        endif
        idecal=nint(zr(icompr+1))
        do 2 i = 1, nbsyst
            do 2 j = 1, nbsyst
                hsr(i,j)=zr(icompr-2+idecal+nbsyst*(i-1)+j)
 2          continue
    endif
    call jedema()
end subroutine
