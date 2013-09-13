subroutine xpoajn(maxfem, ino, lsn, jdirno, prefno,&
                  nfiss, he, nnn, inn, inntot,&
                  nbnoc, nbnofi, inofi, co, iacoo2)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
    character(len=2) :: prefno(4)
    character(len=8) :: maxfem
    integer :: jdirno, nnn, inn, inntot, nbnoc, ino
    integer :: nbnofi, inofi, iacoo2, nfiss, he(nfiss)
    real(kind=8) :: lsn(nfiss), co(3)
!
!            ON AJOUTE UN NOUVEAU NOEUD AU NOUVEAU MAILLAGE X-FEM
!
!   IN
!     INO   : NUMÉRO DU NOEUD OU DU POINT D'INTERSECTION
!     LSN    : LEVEL SETS NORMALES EN INO
!     JDIRNO : ADRESSE DU TABLEAU DIRNO LOCAL
!     PREFNO : PREFERENCES POUR LE NOMAGE DES NOUVELLES ENTITES
!     NFISS  : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT PARENT
!     HE     : VALEURS DE(S) FONCTION(S) HEAVISIDE SUR LE SOUS ÉLÉMENT
!     NNN    : NOMBRE DE NOUVEAU NOEUDS A CREER SUR LA MAILLE PARENT
!     INN    : COMPTEUR LOCAL DU NOMBRE DE NOUVEAUX NOEUDS CREES
!     INNTOT : COMPTEUR TOTAL DU NOMBRE DE NOUVEAUX NOEUDS CREES
!     NBNOC  : NOMBRE DE NOEUDS CLASSIQUES DU MAILLAGE FISSURE
!     NBNOFI : NOMBRE DE NOEUDS SITUES SUR LA FISSURE
!     INOFI  : LISTE DES NOEUDS SITUES SUR LA FISSURE
!     NBNOLA : NOMBRE DE NOEUDS SITUES SUR LA FISSURE AVEC DES LAGS
!     INOLA  : LISTE DES NOEUDS SITUES SUR LA FISSURE AVEC DES LAGS
!     CO     : COORDONNEES  INITIALES DE INO
!     IACOO2 : ADRESSE DES COORDONNES DES NOEUDS DU MAILLAGE FISSURE
!     DDLC :  NOMBRE DE DDLS DE CONTACT DE L'ÉLÉMENT PARENT
!   OUT
!     MAXFEM : NOM DU MAILLAGE FISSURE
!     INN    : COMPTEUR LOCAL DU NOMBRE DE NOUVEAU NOEUDS CREES
!     INNTOT : COMPTEUR TOTAL DU NOMBRE DE NOUVEAU NOEUDS CREES
!     NBNOFI : NOMBRE DE NOEUDS SITUES SUR LA FISSURE
!     INOFI  : LISTE DES NOEUDS SITUES SUR LA FISSURE
!     NBNOLA : NOMBRE DE NOEUDS SITUES SUR LA FISSURE AVEC DES LAGS
!     INOLA  : LISTE DES NOEUDS SITUES SUR LA FISSURE AVEC DES LAGS
!     IACOO2 :  ADRESSE DES COORDONNES DES NOEUDS DU MAILLAGE FISSURE
!
!
    real(kind=8) :: crilsn, minlsn
    integer :: j, ifiss, fiss
    character(len=2) :: nm
    character(len=6) :: chn
    character(len=8) :: valk(2)
    parameter     (crilsn = 1.d-4)
    logical :: lpint
    data          valk /'NOEUDS','XPOAJN'/
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- LPINT EST VRAI SI LE NOEUD DU MAILLAGE X-FEM EST SUR LA FISSURE.
! --- ON ATTACHERA DANS CE CAS CE NOEUDS AU GROUPE NFISSU
    if (ino .lt. 1000) then
        lpint = .false.
        do 10 ifiss = 1, nfiss
            if (lsn(ifiss) .eq. 0.d0) lpint = .true.
10      continue
    else if (ino.gt.1000.and.ino.lt.2000) then
        lpint = .true.
    else if (ino.gt.2000) then
        lpint = .false.
        do 20 ifiss = 1, nfiss
            if (abs(lsn(ifiss)) .lt. crilsn) lpint = .true.
20      continue
    endif
!
    if (lpint) then
        minlsn = r8maem()
        do 50 ifiss = 1, nfiss
!     ON DETECTE LA FISSURE CORESPONDANTE AU POINT D'INTERSECTION
!     ATTENTION, IL PEUT Y AVOIR PLUSIEURS CANDIDAT AU NIV DE L'INTER
            if (abs(lsn(ifiss)) .lt. minlsn .and. he(ifiss) .ne. 0) then
                minlsn = abs(lsn(ifiss))
                fiss = ifiss
            endif
50      continue
        if (he(fiss) .eq. -1) then
            nm=prefno(2)
        else
            nm=prefno(3)
        endif
    else
        nm=prefno(1)
    endif
!
!     COMPTEUR DES NOMS DES NOEUDS
    if (inntot .ge. 999999) then
        call utmess('F', 'XFEM_8', sk=valk(1))
    endif
    inn = inn + 1
    inntot= inntot +1
    ASSERT(inn.le.nnn)
!
    zi(jdirno-1+(2+nfiss)*(inn-1)+1) = ino
    zi(jdirno-1+(2+nfiss)*(inn-1)+2) = nbnoc + inntot
    do 30 ifiss = 1, nfiss
        zi(jdirno-1+(2+nfiss)*(inn-1)+2+ifiss) = he(ifiss)
30  end do
    call codent(inntot, 'G', chn)
!
    call jecroc(jexnom(maxfem//'.NOMNOE', nm//chn))
    do 40 j = 1, 3
        zr(iacoo2-1+3*(nbnoc+inntot-1)+j)=co(j)
40  end do
!       LISTE DES NOEUDS SUR LA FISSURE
    if (lpint) then
        nbnofi=nbnofi+1
        zi(inofi-1+nbnofi)=nbnoc+inntot
!        IF (HE(FISS).EQ.-1.AND.DDLC.GT.0) THEN
!        ATTENTION, IL FAUDRA RÉCUPÉRER DDLC AVEC XPOCMP DS XPOMAX
!       LISTE DES NOEUDS PORTANT DDLS DE CONTACT (COTÉ ESCLAVE)
!          NBNOLA=NBNOLA+1
!          ZI(INOLA-1+NBNOLA)=NBNOC+INNTOT
!        ENDIF
    endif
!
    call jedema()
end subroutine
