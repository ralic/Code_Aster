subroutine jerecu(clas)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterf_types.h"
#include "jeveux_private.h"
#include "asterfort/jjalls.h"
#include "asterfort/jjlidy.h"
#include "asterfort/jxecrb.h"
#include "asterfort/jxliro.h"
    character(len=1), intent(in) :: clas
! ----------------------------------------------------------------------
! MARQUE LIBRES LES ENREGISTREMENTS ASSOCIÉS AUX PETITS OBJETS QUAND
! L'ENSEMBLE DES OBJETS ASSOCIÉS A ETE DETRUIT
!
! IN  CLAS   : NOM DE CLASSE ASSOCIEE
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     ------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
    integer :: istat
    common /istaje/  istat(4)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ic, idco, idcol, idec, idos, idosl
    integer :: jcara, jdate, jdocu, jgenr, jhcod
    integer :: jiadd, jiadm, jlong, jlono, jltyp, jluti, jmarq
    integer :: jorig, jrnom, jtype, jusadi, k, lgl, n
    integer :: nbdet, nbgros, nblim, nbpeti, ncla1, ncla2
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
    aster_logical :: litlec
    common /lficje/  litlec(n)
    common /jusadi/  jusadi(n)
    common /inbdet/  nblim(n),nbgros(n),nbpeti(n)
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    real(kind=8) :: svuse, smxuse
    common /statje/  svuse,smxuse
!     ------------------------------------------------------------------
    aster_logical :: actu
    character(len=1) :: kclas
    integer :: itp(1), jitp, iaditp, iaddi(2), lgbl, iadyn
! DEB ------------------------------------------------------------------
    iaddi(2) = 0
    kclas = clas
    if (kclas .eq. ' ') then
        ncla1 = 1
        ncla2 = index ( classe , '$' ) - 1
        if (ncla2 .lt. 0) ncla2 = n
    else
        ncla1 = index ( classe , kclas)
        ncla2 = ncla1
    endif
    do ic = ncla1, ncla2
        if (nbpeti(ic) .lt. nblim(ic)) cycle
        lgbl = 1024*longbl(ic)*lois
        call jjalls(lgbl, 0, 'V', 'I', lois,&
                    'INIT', itp, jitp, iaditp, iadyn)
        iszon(jiszon+iaditp-1) = istat(2)
        iszon(jiszon+iszon(jiszon+iaditp-4)-4) = istat(4)
        svuse = svuse + (iszon(jiszon+iaditp-4) - iaditp + 4)
        smxuse = max(smxuse,svuse)
!
! ----- DECHARGEMENT DES TAMPONS DE LECTURE ET D'ECRITURE
! ----- AFIN D'ACTUALISER LES ADRESSES DISQUES DES COLLECTIONS
! ----- STOCKEES DANS DES PETITS OBJETS
!
        if (iitecr(ic) .gt. 0) then
            call jxecrb(ic, iitecr(ic), kitecr(ic)+1, lgbl, 0,&
                        0)
            iitecr(ic) = 0
        endif
        if (litlec(ic)) then
            call jxecrb(ic, iitlec(ic), kitlec(ic)+1, lgbl, 0,&
                        0)
            litlec(ic) = .false.
            iitlec(ic) = 0
        endif
!
! ----- BOUCLE "TANT QUE" SUR LES ENREGISTREMENTS UTILISES
!
        k = 1
200     continue
! --------L'ENREGISTREMENT 1 N'EST JAMAIS RECUPERABLE
        k = k + 1
        if (k .le. nbluti(ic)) then
            idco = iusadi(jusadi(ic)+3*k-2)
            idos = iusadi(jusadi(ic)+3*k-1)
            nbdet = iusadi(jusadi(ic)+3*k )
!
! --------L'ENREGISTREMENT CONTIENT DES PETITS OBJETS
! --------ON PARCOURT LE CHAINAGE POUR DETERMINER SI UNE
! --------PARTIE DE L'ENREGISTREMENT EST OCCUPEE
! --------ON EXPLORE L'ENREGISTREMENT
!
            if (idco .eq. 0 .and. idos .eq. 0 .and. nbdet .gt. 0) then
                iaddi(1) = k
                call jxliro(ic, iaditp, iaddi, lgbl)
                actu = .true.
                idec = 0
300             continue
                idcol = iszon(jiszon+iaditp+idec )
                idosl = iszon(jiszon+iaditp+idec+1)
                lgl = iszon(jiszon+iaditp+idec+2)
                if (idcol .eq. 0 .and. idosl .eq. 0) then
                    goto 350
                else if (idcol .lt. 0 .or. idosl .lt. 0) then
                    goto 320
                endif
                actu = .false.
                goto 350
320             continue
                idec = idec+lgl+3
                goto 300
350             continue
                if (actu) then
                    iusadi(jusadi(ic)+3*k-2) = -1
                    iusadi(jusadi(ic)+3*k-1) = -1
                    iusadi(jusadi(ic)+3*k ) = 0
                endif
            endif
            goto 200
        endif
        call jjlidy(iadyn, iaditp)
        nbpeti(ic) = 0
    end do
! FIN ------------------------------------------------------------------
end subroutine
