subroutine jxcopy(clsinz, nominz, clsouz, nmoutz, nbext)
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
#include "jeveux_private.h"
#include "asterc/cpfile.h"
#include "asterc/readdr.h"
#include "asterc/rmfile.h"
#include "asterc/writdr.h"
#include "asterfort/codent.h"
#include "asterfort/jeinif.h"
#include "asterfort/jjalls.h"
#include "asterfort/jjlidy.h"
#include "asterfort/jxferm.h"
#include "asterfort/jxouvr.h"
#include "asterfort/lxmins.h"
#include "asterfort/u2mess.h"
    character(len=*) :: clsinz, nominz, clsouz, nmoutz
    character(len=1) :: clasin, clasou
    character(len=8) :: nomin, nomout
!     ------------------------------------------------------------------
!     RECOPIE D'UNE BASE DE DONNEES APRES ELIMINATION DES
!     ENREGISTREMENTS DEVENUS INACCESSIBLES
!     ------------------------------------------------------------------
! IN  CLSINZ : NOM DE CLASSE ASSOCIEE EN ENTREE
! IN  NOMINZ : NOM DE LA BASE EN ENTREE
! IN  CLSOUZ : NOM DE CLASSE ASSOCIEE EN SORTIE
! IN  NMOUTZ : NOM DE LA BASE EN SORTIE
! OUT NBEXT  : NOMBRE D'"EXTENDS" UTILISES APRES RETASSAGE
!     ------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     ------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: istat
    common /istaje/  istat(4)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadloc, iadyn, ib, ici, ico, ierr, k
    integer :: lbloc, n, nbext, nbloc, nrep, numext
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
!
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
    integer :: idn, iext, nbenrg
    common /iextje/  idn(n) , iext(n) , nbenrg(n)
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    real(kind=8) :: svuse, smxuse
    common /statje/  svuse,smxuse
    character(len=128) :: repglo, repvol
    common /banvje/  repglo,repvol
    integer :: lrepgl, lrepvo
    common /balvje/  lrepgl,lrepvo
!     ------------------------------------------------------------------
    character(len=1) :: kclas
    character(len=8) :: nomba1, nomba2, nom
    character(len=128) :: noml1, noml2
    integer :: itp(1), jitp, iaditp, lgbl1, lgbl2, info, l1, l2
! DEB ------------------------------------------------------------------
    nomin = nominz
    clasin = clsinz
    nomout = nmoutz
    clasou = clsouz
!
    kclas = clasin
    call jeinif('POURSUITE', 'DETRUIT', nomin, kclas, 1,&
                1, 1)
    ici = index ( classe , kclas)
    kclas = clasou
    nrep = nremax(ici)
    nbloc= nbenrg(ici)
    lbloc= longbl(ici)
    nom = nomout(1:4)//'.?  '
    call lxmins(nom)
    if (nom(1:4) .eq. 'glob') then
        noml1=repglo(1:lrepgl)//'/'//nom
    else if (nom(1:4) .eq. 'vola') then
        noml1=repvol(1:lrepvo)//'/'//nom
    else
        noml1='./'//nom
    endif
    info = 1
    call rmfile(noml1, info)
    call jeinif('DEBUT', 'SAUVE', nomout, kclas, nrep,&
                nbloc, lbloc)
    ico = index ( classe , kclas)
    nomba1 = nomfic(ici)(1:4)//'.   '
    nomba2 = nomfic(ico)(1:4)//'.   '
!
    lgbl1= 1024*longbl(ici)*lois
    lgbl2= 1024*longbl(ico)*lois
    call jjalls(lgbl1, 0, ' ', 'I', lois,&
                'INIT', itp, jitp, iaditp, iadyn)
    iszon(jiszon+iaditp-1) = istat(1)
    iszon(jiszon+iszon(jiszon+iaditp-4)-4) = istat(4)
    svuse = svuse + (iszon(jiszon+iaditp-4) - iaditp + 4)
    smxuse = max(smxuse,svuse)
    do 50 k = 1, (nbluti(ici)-1)/nbenrg(ici)
        call jxouvr(ico, k+1)
        iext(ico) = iext(ico) + 1
50  end do
!
    if (nomba1(1:4) .eq. 'glob') then
        noml1=repglo(1:lrepgl)//'/'//nomba1
        l1=lrepgl+1
    else if (nomba1(1:4) .eq. 'vola') then
        noml1=repvol(1:lrepvo)//'/'//nomba1
        l1=lrepvo+1
    else
        noml1='./'//nomba1
        l1=2
    endif
    if (nomba2(1:4) .eq. 'glob') then
        noml2=repglo(1:lrepgl)//'/'//nomba2
        l2=lrepgl+1
    else if (nomba2(1:4) .eq. 'vola') then
        noml2=repvol(1:lrepvo)//'/'//nomba2
        l2=lrepvo+1
    else
        noml2='./'//nomba2
        l2=2
    endif
    do 100 k = 1, nbluti(ici)
        numext = (k-1)/nbenrg(ici)
        iadloc = k - (numext*nbenrg(ici))
        call codent(numext+1, 'G', noml1(l1+6:l1+7))
        call readdr(noml1, iszon(jiszon+iaditp), lgbl1, iadloc, ierr)
        if (ierr .ne. 0) then
            call u2mess('F', 'JEVEUX_47')
        endif
        call codent(numext+1, 'G', noml2(l2+6:l2+7))
        call writdr(noml2, iszon(jiszon + iaditp), lgbl2, iadloc, - 1,&
                    ib, ierr)
        if (ierr .ne. 0) then
            call u2mess('F', 'JEVEUX_48')
        endif
100  end do
    nbext = numext+1
    call jxferm(ici)
    call jxferm(ico)
    call jjlidy(iadyn, iaditp)
    classe(ico:ico) = ' '
    classe(ici:ici) = ' '
!
! Destruction de tous les receptacles avant recopie.
!
    noml1(l2+6:l2+7)='.*'
    call rmfile (noml1, info)

    do 300 k = 1, nbext
        call codent(k, 'G', noml2(l2+6:l2+7))
        call codent(k, 'G', noml1(l1+6:l1+7))
        call cpfile('M', noml2, noml1)
300  end do
! FIN ------------------------------------------------------------------
end subroutine
