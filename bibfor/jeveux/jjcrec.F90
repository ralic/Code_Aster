subroutine jjcrec(icl, ida, genri, typei, nb,&
                  iadmi)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "jeveux_private.h"
#include "asterfort/jjallt.h"
#include "asterfort/jjecrs.h"
#include "asterfort/jjprem.h"
#include "asterfort/utmess.h"
    integer :: icl, ida, nb, iadmi
    character(len=*) :: genri, typei
! ----------------------------------------------------------------------
! CREATION D'UN OBJET SIMPLE ATTRIBUT COMPOSANT UNE COLLECTION
!
! IN  ICL   : CLASSE ASSOCIEE
! IN  IDA   : IDENTIFICATEUR
! IN  GENRI : GENRE DE L'OS
! IN  TYPEI : TYPE DE L'OS
! IN  NB    : LONGEUR EN MOTS DU SEGMENT DE VALEUR ASSOCIE
! OUT IADMI : ADRESSE DU SEGMENT DE VALEUR DANS ISZON
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!
!-----------------------------------------------------------------------
    integer :: iadyn, iv, jcara, jdate, jdocu, jgenr, jhcod
    integer :: jiadd, jiadm, jlong, jlono, jltyp, jluti
    integer :: jmarq, jorig, jrnom, jtype, l, lonoi, n
    integer :: nbl, nhc
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!     ------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
!     ------------------------------------------------------------------
    character(len=4) :: ifmt
    integer :: ilorep, ideno, ilnom, ilmax, iluti, idehc
    parameter      ( ilorep=1,ideno=2,ilnom=3,ilmax=4,iluti=5,idehc=6)
    integer :: irt
! DEB ------------------------------------------------------------------
    irt = 0
    genr(jgenr (icl) + ida ) = genri(1:1)
    type(jtype (icl) + ida ) = typei(1:1)
    if (genri .eq. 'N' .and. typei(1:1) .ne. 'K') then
        call utmess('F', 'JEVEUX1_38')
    endif
    if (typei(1:1) .eq. 'K') then
        l = len(typei)
        if (l .eq. 1) then
            call utmess('F', 'JEVEUX1_39')
        endif
        write(ifmt,'(''(I'',I1,'')'')') l - 1
        read ( typei(2:l) , ifmt ) iv
        if (iv .le. 0 .or. iv .gt. 512) then
            call utmess('F', 'JEVEUX1_40', si=iv)
        endif
        if (genri .eq. 'N') then
            if (mod ( iv , lois ) .ne. 0) then
                call utmess('F', 'JEVEUX1_41', si=iv)
            endif
            if (iv .gt. 24) then
                call utmess('F', 'JEVEUX1_42', si=iv)
            endif
        endif
    else if (typei(1:1) .eq. 'S') then
        iv = lor8/2
    else if (typei(1:1) .eq. 'I') then
        iv = lois
    else if (typei(1:1) .eq. 'R') then
        iv = lor8
    else if (typei(1:1) .eq. 'C') then
        iv = loc8
    else if (typei(1:1) .eq. 'L') then
        iv = lols
    else
        call utmess('F', 'JEVEUX1_43', sk=typei(1:1))
    endif
    ltyp(jltyp (icl) + ida ) = iv
    iadm(jiadm (icl) + 2*ida-1 ) = 0
    iadm(jiadm (icl) + 2*ida ) = 0
    if (nb .gt. 0) then
        if (genri .eq. 'N') then
            long ( jlong(icl) + ida ) = nb
            lonoi = (idehc + jjprem(nb,irt)) * lois + (nb+1) * iv
            if (mod(lonoi,iv) .gt. 0) then
                lono ( jlono(icl) + ida ) = (lonoi / iv) + 1
            else
                lono ( jlono(icl) + ida ) = (lonoi / iv)
            endif
        else if (typei(1:1) .eq. 'C') then
            long(jlong (icl) + ida ) = 2 * nb
            lono(jlono (icl) + ida ) = 2 * nb
        else
            long(jlong (icl) + ida ) = nb
            lono(jlono (icl) + ida ) = nb
        endif
        nbl = lono(jlono (icl) + ida ) * iv
        call jjallt(nbl, icl, genri, typei, iv,&
                    'INIT', iadmi, iadyn)
        iadm(jiadm (icl) + 2*ida-1 ) = iadmi
        iadm(jiadm (icl) + 2*ida ) = iadyn
        call jjecrs(iadmi, icl, ida, 0, 'E',&
                    imarq(jmarq(icl)+2*ida-1))
        if (genri .eq. 'N') then
            nhc = jjprem(nb,irt)
            iszon(jiszon + iadmi - 1 + ilorep ) = nhc
            iszon(jiszon + iadmi - 1 + ideno ) = (idehc+nhc)*lois
            iszon(jiszon + iadmi - 1 + ilnom ) = iv
            iszon(jiszon + iadmi - 1 + ilmax ) = nb
            iszon(jiszon + iadmi - 1 + iluti ) = 0
            iszon(jiszon + iadmi - 1 + idehc ) = idehc
        endif
    else if (genri .eq. 'E') then
        long(jlong (icl) + ida ) = 1
    endif
! FIN ------------------------------------------------------------------
end subroutine
