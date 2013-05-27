subroutine mecoe1(opt, te)
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterc/indik8.h'
    include 'asterfort/modatt.h'
    include 'asterfort/nbpara.h'
    include 'asterfort/nopara.h'
    integer :: opt, te
! ----------------------------------------------------------------------
!     ENTREES:
!      OPT : OPTION
!      TE  : TYPE D'ELEMENT
!
!     SORTIES:
!      MISE A JOUR DU COMMON  CAII06 (OBJET .IA_CHLO2)
!
! ----------------------------------------------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    integer :: iadsgd, iamloc, iaopds, iaopmo, iaopno, iaoppa, iaoptt
    integer :: icode, ilmloc, ilopmo, ilopno
    integer :: iparg, lgco, m2, modloc, nbpoin, nbscal, npara, nparin
    integer :: ipar, npario
!-----------------------------------------------------------------------
    character(len=8) :: nompar
!
! DEB-------------------------------------------------------------------
!
!
!     -- ON MET A "-1" LGCATA POUR LES PARAMETRES INCONNUS
!        DES TYPE_ELEMENT
    do 10,iparg = 1,npario
    zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)=-1
    10 end do
!
!
    npara = nbpara(opt,te,'IN ')
    do 20 ipar = 1, npara
        nompar = nopara(opt,te,'IN ',ipar)
        iparg = indik8(zk8(iaoppa),nompar,1,npario)
        m2 = modatt(opt,te,'IN ',ipar)
        modloc = iamloc - 1 + zi(ilmloc-1+m2)
        icode = zi(modloc-1+1)
        nbscal = zi(modloc-1+3)
        if (icode .le. 3) then
            nbpoin = zi(modloc-1+4)
        else
            nbpoin = 0
        endif
!
        zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+1)=m2
        zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)=nbscal
        zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+3)=nbpoin
20  end do
!
!
    npara = nbpara(opt,te,'OUT')
    do 30 ipar = 1, npara
        nompar = nopara(opt,te,'OUT',ipar)
        iparg = indik8(zk8(iaoppa),nompar,1,npario)
        m2 = modatt(opt,te,'OUT',ipar)
        modloc = iamloc - 1 + zi(ilmloc-1+m2)
        icode = zi(modloc-1+1)
        nbscal = zi(modloc-1+3)
        if (icode .le. 3) then
            nbpoin = zi(modloc-1+4)
        else
            nbpoin = 0
        endif
!
        zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+1)=m2
        zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)=nbscal
        zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+3)=nbpoin
30  end do
!
end subroutine
