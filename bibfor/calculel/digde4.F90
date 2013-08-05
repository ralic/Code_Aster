function digde4(igr, iel, opt, ipar, dim1,&
                codvoi)
    implicit none
    integer :: digde4
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!     ENTREES:
!        IEL : NUMERO DEL'ELEMENT DANS LE GREL
!        ...
!     SORTIES:
!        DIGDE4 : DIMENSION DE LA MATRICE ELEMENTAIRE POUR IEL
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/digde3.h"
#include "asterfort/modatt.h"
#include "asterfort/voiuti.h"
    integer :: igr, iel, opt, ipar, dim1
    character(len=16) :: codvoi
    integer :: nvoima, nscoma, nbvois
    parameter(nvoima=100,nscoma=4)
    integer :: livois(1:nvoima), tyvois(1:nvoima), nbnovo(1:nvoima)
    integer :: nbsoco(1:nvoima), lisoco(1:nvoima, 1:nscoma, 1:2)
    integer :: iamaco, ilmaco, iamsco, ilmsco, ialiel, illiel
!
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: lon1, numa, numail
    integer :: numav, igr2, te2, mode2, dim2
    integer :: kvois, nel2p1
!
    numail(igr,iel) = zi(ialiel-1+zi(illiel+igr-1)+iel-1)
!
! DEB-------------------------------------------------------------------
    ASSERT(evfini.eq.1)
    lon1=0
    numa = numail(igr,iel)
    call voiuti(numa, codvoi, nvoima, nscoma, jrepe,&
                jptvoi, jelvoi, nbvois, livois, tyvois,&
                nbnovo, nbsoco, lisoco)
    do 22, kvois=1,nbvois
    numav=livois(kvois)
    igr2=zi(jrepe-1+2*(numav-1)+1)
    nel2p1=zi(illiel-1+igr2+1)-zi(illiel-1+igr2)
    te2= zi(ialiel-1+zi(illiel-1+igr2)-1+nel2p1)
    mode2 = modatt(opt,te2,'OUT',ipar)
    ASSERT(mode2.gt.0)
    dim2=digde3(mode2,'L')
    lon1=lon1+dim1*dim2
    22 end do
    lon1=lon1+dim1*dim1
    digde4=lon1
end function
