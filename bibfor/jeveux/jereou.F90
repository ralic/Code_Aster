subroutine jereou(clas, pcent)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
    include 'asterc/rmfile.h'
    include 'asterfort/assert.h'
    include 'asterfort/jeinif.h'
    include 'asterfort/jelibf.h'
    include 'asterfort/lxmins.h'
    include 'asterfort/u2mesg.h'
    character(len=*) :: clas
    real(kind=8) :: pcent
! ----------------------------------------------------------------------
! ROUTINE UTILISATEUR PERMETTANT DE FERMER UNE BASE, DE DETRUIRE LE OU
!         LES FICHIERS ASSOCIES ET DE LA RE-OUVRIR
! IN : CLAS  NOM DE LA CLASSE ASSOCIEE ('G','V', ...)
! IN : PCENT POURCENTAGE EN TERME DE NOMBRE D'ENREGISTREMENTS OCCUPES
!            AU-DELA DUQUEL ON DECLENCHE LA DESTRUCTION
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
!
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
!
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    character(len=8) :: nombas
    common /kbasje/  nombas(n)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! DEB ------------------------------------------------------------------
    character(len=1) :: klas
    character(len=8) :: kstin, kstou, nom, nomb
    integer :: ic, nrep, lbloc, nbloc, info, vali(3)
    real(kind=8) :: valr(2)
!
    klas = clas
    call assert(klas .ne. ' ')
    ic = index (classe , klas)
!
    nomb = nombas(ic)
    nom = nomb(1:4)//'.?  '
    nrep = nremax(ic)
    lbloc = longbl(ic)
    nbloc = nblmax(ic)
    kstin = kstini(ic)
    kstou = kstout(ic)
!
    if (nbluti(ic) .gt. pcent*nblmax(ic)) then
        valr (1) = 100.d0*nbluti(ic)/nblmax(ic)
        valr (2) = 100.d0*pcent
        vali (1) = nbluti(ic)
        vali (2) = nint(nbluti(ic)*longbl(ic)*lois/1024.d0)
        vali (3) = nblmax(ic)
        call u2mesg('I', 'JEVEUX_63', 1, nombas(ic), 3,&
                    vali, 2, valr)
        info = 0
        call jelibf('DETRUIT', klas, info)
        call lxmins(nom)
        call rmfile(nom, 0)
        call jeinif(kstin, kstou, nomb, klas, nrep,&
                    nbloc, lbloc)
    endif
!
end subroutine
