subroutine jxlirb(ic, iaddi, iadmo, lso)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
! aslint: disable=
    implicit none
    include 'jeveux_private.h'
    include 'asterc/readdr.h'
    include 'asterfort/codent.h'
    include 'asterfort/u2mesg.h'
    integer :: ic, iaddi, iadmo, lso
! ----------------------------------------------------------------------
! LECTURE D'UN BLOC DU FICHIER D'ACCES DIRECT ASSOCIE A UNE BASE
!
!
! IN  IC    : CLASSE ASSOCIEE
! IN  IADDI : ADRESSE DISQUE
! IN  IADMO : ADRESSE MEMOIRE EN OCTETS
! IN  LSO   : LONGUEUR EN OCTETS
! ----------------------------------------------------------------------
!
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     ------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadloc, ierr, jiacce, jiecr, n
    integer :: nbacce, nblent, numext
    real(kind=8) :: r8bid
!-----------------------------------------------------------------------
    parameter      ( n = 5 )
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
!
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    character(len=8) :: nombas
    common /kbasje/  nombas(n)
    character(len=128) :: repglo, repvol
    common /banvje/  repglo,repvol
    integer :: lrepgl, lrepvo
    common /balvje/  lrepgl,lrepvo
    integer :: idn, iext, nbenrg
    common /iextje/  idn(n) , iext(n) , nbenrg(n)
    common /jiacce/  jiacce(n),nbacce(2*n)
!     ------------------------------------------------------------------
    logical :: lrab
    character(len=8) :: nom
    character(len=128) :: nom128
    integer :: lgbl, vali(3)
! DEB ------------------------------------------------------------------
    ierr = 0
    lgbl = 1024*longbl(ic)*lois
    nblent = lso / lgbl
    lrab = ( mod (lso,lgbl) .ne. 0 )
!
    if (kstini(ic) .ne. 'DUMMY   ') then
        nom = nomfic(ic)(1:4)//'.   '
        do 10 i = 1, nblent
            numext = (iaddi+i-2)/nbenrg(ic)
            iadloc = (iaddi+i-1)-(numext*nbenrg(ic))
            call codent(numext+1, 'G', nom(6:7))
            if (nom(1:4) .eq. 'glob') then
                nom128=repglo(1:lrepgl)//'/'//nom
            else if (nom(1:4) .eq. 'vola') then
                nom128=repvol(1:lrepvo)//'/'//nom
            else
                nom128='./'//nom
            endif
            jiecr = (jk1zon+iadmo-1+lgbl*(i-1))/lois+1
            call readdr(nom128, iszon(jiecr), lgbl, iadloc, ierr)
            if (ierr .ne. 0) then
                vali(1) = iaddi+i-1
                vali(2) = numext
                vali(3) = ierr
                call u2mesg('F', 'JEVEUX_41', 1, nombas(ic), 3,&
                            vali, 0, r8bid)
            endif
            nbacce(2*ic-1) = nbacce(2*ic-1) + 1
10      continue
        iacce (jiacce(ic)+iaddi)=iacce(jiacce(ic)+iaddi) + 1
        if (lrab) then
            numext = (iaddi+nblent-1)/nbenrg(ic)
            iadloc = (iaddi+nblent)-(numext*nbenrg(ic))
            call codent(numext+1, 'G', nom(6:7))
            if (nom(1:4) .eq. 'glob') then
                nom128=repglo(1:lrepgl)//'/'//nom
            else if (nom(1:4) .eq. 'vola') then
                nom128=repvol(1:lrepvo)//'/'//nom
            else
                nom128='./'//nom
            endif
            jiecr = (jk1zon+iadmo-1+lso-lgbl)/lois+1
            if (lso .lt. lgbl) then
                jiecr = (jk1zon+iadmo-1)/lois+1
            endif
            call readdr(nom128, iszon(jiecr), lgbl, iadloc, ierr)
            if (ierr .ne. 0) then
                vali(1) = iaddi+i-1
                vali(2) = numext
                vali(3) = ierr
                call u2mesg('F', 'JEVEUX_41', 1, nombas(ic), 3,&
                            vali, 0, r8bid)
            endif
            nbacce(2*ic-1) = nbacce(2*ic-1) + 1
        endif
    endif
! FIN ------------------------------------------------------------------
end subroutine
