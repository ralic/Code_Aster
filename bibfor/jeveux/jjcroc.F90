subroutine jjcroc(knat, icre)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/jjcodn.h"
#include "asterfort/jjprem.h"
#include "asterfort/utmess.h"
    integer :: icre
    character(len=8) :: knat
! ----------------------------------------------------------------------
! INSERTION D'UN NOM DANS UN REPERTOIRE PRIVE
! ACTUALISE LE CONTENU DU COMMON  /IATCJE/
!
! IN  KNAT   : CHAINE VALANT '$$XNOM   ' OU '$$XNUM   '
! IN  ICRE   : CODE DE CREATION DU NOM PASSE A JJCODN
!              ICRE = 0 ON VERIFIE UNIQUEMENT L'EXISTENCE
!              ICRE = 1 ON CREE LE NOM
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadmi, ibacol, ibnom, ibnum, ic, idoc
    integer :: ixnom, ixnum, jcara, jdate, jdocu, jgenr, jhcod
    integer :: jiadd, jiadm, jlong, jlono, jltyp, jluti, jmarq
    integer :: jorig, jrnom, jtype, kadm, kitab, longno, ltypi
    integer :: n, nhcod, nmax, nuti, nutiex
!-----------------------------------------------------------------------
    parameter      ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! ----------------------------------------------------------------------
    integer :: numec
    common /inumje/  numec
    character(len=24) :: nomec
    common /knomje/  nomec
!
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
! ----------------------------------------------------------------------
    integer :: idnom, idnum
    parameter    (  idnom  = 5  , idnum  = 10 )
! ----------------------------------------------------------------------
    integer :: ilorep, ideno, ilnom, ilmax, iluti, idehc
    parameter      ( ilorep=1,ideno=2,ilnom=3,ilmax=4,iluti=5,idehc=6)
! ----------------------------------------------------------------------
    character(len=24) :: nom
    integer :: jitab, irt
!
    character(len=8) :: nume, nome
    data             nume       , nome&
     &               / '$$XNUM  ' , '$$XNOM  ' /
! DEB ------------------------------------------------------------------
    irt = 0
    if (knat .eq. '        ') then
!
! ------ REPERTOIRE DE NOM
!
        ltypi = ltyp (jltyp(iclaos) + idatos )
        iadmi = iadm( jiadm(iclaos) + 2*idatos-1 )
        if (iadmi .ne. 0) then
            kadm = iadmi
            jitab = jiszon + kadm - 1
            kitab = jk1zon + ( kadm - 1 ) * lois
            nmax = long (jlong(iclaos)+idatos )
            nuti = luti (jluti(iclaos)+idatos )
            nutiex = nuti
            nom = rnom (jrnom(iclaos)+idatos )(1:24)
            if (nuti .eq. 0) then
                nhcod = jjprem(nmax,irt)
                iszon(jitab + ilorep) = nhcod
                iszon(jitab + ideno ) = ( idehc + nhcod ) * lois
                iszon(jitab + ilnom ) = ltypi
                iszon(jitab + ilmax ) = nmax
                iszon(jitab + iluti ) = nuti
                iszon(jitab + idehc ) = idehc
                do 5 i = 1, nhcod
                    iszon( jitab + i + idehc ) = 0
 5              continue
                do 6 i = 1, nmax*ltypi
                    k1zon( kitab + iszon(jitab+ideno) + i ) = '?'
 6              continue
            endif
        endif
        idoc = jjcodn ( icre, nom, nomec(1:ltypi), iszon(jitab+1), k1zon(kitab+1), nmax, nuti )
        if (idoc .gt. 0) then
            idatoc = idoc
            nomoc = nomec
        else
            idatoc = 0
            nomoc = bl32
        endif
        nomco = '$$$$$$$$$$$$$$$$$$$$$$$$'
        if (nutiex .ne. nuti) luti (jluti(iclaos)+idatos) = nuti
    else
!
! ----- COLLECTION
!
        ibacol = iadm(jiadm(iclaco)+2*idatco-1)
        ic = iclaco
        if (knat .eq. nume) then
!
! --------  ACCES PAR NUMERO
!
            ixnum = iszon(jiszon+ibacol+idnum )
            ixnom = iszon(jiszon+ibacol+idnom )
            if (ixnum .ne. 0) then
                ibnum = iadm(jiadm(ic)+2*ixnum-1)
                nmax = iszon(jiszon+ibnum )
                nuti = iszon(jiszon+ibnum+1)
            else if (ixnom .ne. 0) then
                if (icre .gt. 0) then
                    call utmess('F', 'JEVEUX1_44')
                endif
                nmax = long ( jlong(ic) + ixnom )
                nuti = luti ( jluti(ic) + ixnom )
            endif
            if (numec .le. 0 .or. numec .gt. nmax) then
                call utmess('F', 'JEVEUX_38', si=numec)
            endif
            nutiex = nuti
            idatoc = 0
            if (icre .gt. 0) then
                if (numec .le. nutiex) then
                    call utmess('F', 'JEVEUX1_45', si=numec)
                else
                    if (nutiex .lt. nmax) then
                        nuti = nuti + 1
                        iszon(jiszon+ibnum+1) = nuti
                    else
                        call utmess('F', 'JEVEUX1_46')
                    endif
                endif
            endif
            idatoc = numec
        else if (knat .eq. nome) then
!
! --------  ACCES PAR NOM
!
            ixnom = iszon(jiszon+ibacol+idnom )
            if (ixnom .eq. 0) then
                call utmess('F', 'JEVEUX1_47')
            else
                nmax = long (jlong(ic)+ixnom )
                nuti = luti (jluti(ic)+ixnom )
                nutiex = nuti
                if (nomec .ne. nomoc .or. icre .eq. -3) then
                    ibnom = iadm ( jiadm(ic) + 2*ixnom-1 )
                    jitab = jiszon + ibnom - 1
                    kitab = jk1zon + ( ibnom - 1 ) * lois
                    nom = rnom (jrnom(ic)+ixnom )(1:24)
                    longno = ltyp (jltyp(ic)+ixnom )
                    if (nuti .eq. 0) then
                        nhcod = jjprem(nmax,irt)
                        iszon(jitab + ilorep) = nhcod
                        iszon(jitab + ideno ) = ( idehc + nhcod ) * lois
                        iszon(jitab + ilnom ) = longno
                        iszon(jitab + ilmax ) = nmax
                        iszon(jitab + iluti ) = nuti
                        iszon(jitab + idehc ) = idehc
                        do 15 i = 1, nhcod
                            iszon( jitab + i + idehc ) = 0
15                      continue
                        do 16 i = 1, nmax * longno
                            k1zon( kitab + iszon(jitab+ideno) + i) =&
                            '?'
16                      continue
                    endif
                    idoc = jjcodn (&
                           icre, nom, nomec(1:longno), iszon(jitab+1), k1zon(kitab+1), nmax, nuti&
                           )
                    if (idoc .gt. 0) then
                        nomoc = nomec
                        idatoc = idoc
                    else
                        nomoc = bl32
                        idatoc = 0
                    endif
                    if (nutiex .ne. nuti) luti (jluti(ic)+ixnom ) = nuti
                endif
            endif
        endif
    endif
! FIN ------------------------------------------------------------------
end subroutine
