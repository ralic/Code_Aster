subroutine jelibz(clas)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux_private.h"
#include "asterfort/jjlide.h"
    character(len=*) :: clas
! ----------------------------------------------------------------------
! LIBERATION DE L'ENSEMBLE DES OBJETS MARQUES PAR -1
!
! IN  CLAS   : CLASSE DES OBJETS A LIBERER
!
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: jcara, jdate, jdocu, jgenr, jhcod, jiadd, jiadm
    integer :: jlong, jlono, jltyp, jluti, jmarq, jorig, jrnom
    integer :: jtype, n, nmax
!-----------------------------------------------------------------------
    parameter      ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
    integer :: ivnmax, idiadm, idmarq, idnum
    parameter    ( ivnmax = 0   , idiadm = 3 ,&
     &               idmarq = 4   ,&
     &                 idnum  = 10 )
! ----------------------------------------------------------------------
    integer :: ncla1, ncla2, ibacol, ibmarq, ic, id, iret, ix
    integer :: j, k, marqi, iclasi
    character(len=32) :: crnom, d32
    character(len=1) :: kclas
    data             d32 /'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/
! DEB ------------------------------------------------------------------
    kclas = clas
    iclasi = iclas
    if (kclas .eq. ' ') then
        ncla1 = 1
        ncla2 = index ( classe , '$' ) - 1
        if (ncla2 .lt. 0) ncla2 = n
    else
        ncla1 = index ( classe , kclas)
        ncla2 = ncla1
    endif
    do 100 ic = ncla1, ncla2
        do 150 j = 1, nremax(ic)
            crnom = rnom(jrnom(ic)+j)
            if (crnom(1:1) .eq. '?' .or. crnom(25:26) .eq. '$$') goto 150
!          CALL JJCREN ( CRNOM , 0 , IRET )
            if (genr(jgenr(ic)+j) .eq. 'X') then
                iclas = ic
                iclaco = ic
                idatco = j
                nomco = crnom
                nomoc = d32
                if (iclasi .ne. iclaco) then
                    nomos = d32
                endif
                ibacol = iadm(jiadm(ic)+2*j-1)
                if (ibacol .eq. 0) goto 150
                id = iszon(jiszon + ibacol + idiadm)
                if (id .gt. 0) then
!
! ------------- COLLECTION DISPERSEE ( OBJETS DE COLLECTION )
!
                    ix = iszon(jiszon + ibacol + idmarq)
                    ibmarq = iadm(jiadm(ic)+2*ix-1)
                    nmax = iszon(jiszon+ibacol+ivnmax )
                    do 170 k = 1, nmax
                        marqi = iszon(jiszon+ibmarq-1+2*k-1)
                        if (marqi .eq. -1) then
                            call jjlide('JELIBZ', crnom, 2)
                            goto 171
                        endif
170                  continue
                endif
!
! ---------- COLLECTION CONTIGUE OU DISPERSEE ( OBJETS ATTRIBUTS )
!
                do 162 k = idnum, 1, -1
                    id = iszon(jiszon + ibacol + k)
                    if (id .gt. 0) then
                        marqi = imarq(jmarq(ic)+2*id-1)
                        if (marqi .eq. -1) then
                            call jjlide('JELIBZ', crnom, 2)
                            goto 171
                        endif
                    endif
162              continue
171              continue
            else
!
! --------- OBJET SIMPLE
!
                iclas = ic
                iclaos = ic
                idatos = j
                nomos = crnom
                if (iclasi .ne. iclaos) then
                    nomco = d32
                    nomoc = d32
                endif
                marqi = imarq(jmarq(ic)+2*j-1)
                if (marqi .eq. -1) then
                    call jjlide('JELIBZ', crnom, 1)
                endif
            endif
150      continue
100  end do
! FIN ------------------------------------------------------------------
end subroutine
