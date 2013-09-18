subroutine deltau(jrwork, jnbpg, nbpgt, nbordr, ordini,&
                  nmaini, nbmap, numpaq, tspaq, nommet,&
                  nomcri, nomfor, grdvie, forvie, forcri, cesr)
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
! person_in_charge: van-xuan.tran at edf.fr
    implicit none
#include "jeveux.h"
#include "asterc/loisem.h"
#include "asterc/lor8em.h"
#include "asterc/r8pi.h"
#include "asterfort/acgrdo.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedisp.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcpare.h"
#include "asterfort/rnomat.h"
#include "asterfort/utmess.h"
#include "asterfort/vecnuv.h"
#include "asterfort/wkvect.h"
    integer :: jrwork, jnbpg, nbpgt, nbordr, nmaini, numpaq, nbmap
    integer :: tspaq, ordini
    character(len=8) :: grdvie
    character(len=16) :: nomcri, nommet, nomfor, forvie, forcri
    character(len=19) :: cesr
! ---------------------------------------------------------------------
! BUT: DETERMINER LE PLAN INCLINE POUR LEQUEL DELTA_TAU EST MAXIMUM
!      POUR CHAQUE POINT DE GAUSS D'UN <<PAQUET>> DE MAILLES.
! ---------------------------------------------------------------------
! ARGUMENTS:
! JRWORK     IN    I  : ADRESSE DU VECTEUR DE TRAVAIL CONTENANT
!                       L'HISTORIQUE DES TENSEURS DES CONTRAINTES
!                       ATTACHES A CHAQUE POINT DE GAUSS DES MAILLES
!                       DU <<PAQUET>> DE MAILLES.
! JNBPG      IN    I  : ADRESSE DU VECTEUR CONTENANT LE NOMBRE DE
!                       POINT DE GAUSS DE CHAQUE MAILLE DU MAILLAGE.
! NBPGT      IN    I  : NOMBRE TOTAL DE POINTS DE GAUSS A TRAITER.
! NBORDR     IN    I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
!                       STRUCTURE DE DONNEES RESULTAT.
! ORDINI     IN    I  : ORDRE INITIAL POUR LE CHARGEMENT CYCLIQUE
! NMAINI     IN    I  : NUMERO DE LA 1ERE MAILLE DU <<PAQUET>> DE
!                       MAILLES COURANT.
! NBMAP      IN    I  : NOMBRE DE MAILLES DANS LE <<PAQUET>> DE
!                       MAILLES COURANT.
! NUMPAQ     IN    I  : NUMERO DU PAQUET DE MAILLES COURANT.
! TSPAQ      IN    I  : TAILLE DU SOUS-PAQUET DU <<PAQUET>> DE MAILLES
!                       COURANT.
! NOMMET     IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
!                       CIRCONSCRIT.
! NOMCRI     IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
! CESR       IN    K19: NOM DU CHAMP SIMPLE DESTINE A RECEVOIR LES
!                       RESULTATS.
!
! REMARQUE :
!  - LA TAILLE DU SOUS-PAQUET EST EGALE A LA TAILLE DU <<PAQUET>> DE
!    MAILLES DIVISEE PAR LE NOMBRE DE NUMERO D'ORDRE (NBORDR).
!-----------------------------------------------------------------------
!
    integer :: kwork, jcerd, jcerl, jcerv, jad
    integer :: iret, imap, icesd, icesl, icesv, ibid
    integer :: ipg
    integer :: nbpg, sompgw, nbpgp, l
    integer :: icmp 
    real(kind=8) :: vala, valb, coefpa, vresu2(24), valpar(35)
    integer :: icodwo
    character(len=8) :: chmat1, nommat
    character(len=10) :: optio
    character(len=19) :: chmat, cesmat
!
!
!-----------------------------------------------------------------------
!234567                                                              012
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    call jemarq()
!
! !
! OBTENTION DES ADRESSES '.CESD', '.CESL' ET '.CESV' DU CHAMP SIMPLE
! DESTINE A RECEVOIR LES RESULTATS : DTAUM, ....
!
    call jeveuo(cesr//'.CESD', 'L', jcerd)
    call jeveuo(cesr//'.CESL', 'E', jcerl)
    call jeveuo(cesr//'.CESV', 'E', jcerv)
! !
! !
! RECUPERATION MAILLE PAR MAILLE DU MATERIAU DONNE PAR L'UTILISATEUR
!
    call getvid(' ', 'CHAM_MATER', scal=chmat1, nbret=iret)
    chmat = chmat1//'.CHAMP_MAT'
    cesmat = '&&DELTAU.CESMAT'
    call carces(chmat, 'ELEM', ' ', 'V', cesmat,&
                'A', iret)
    call jeveuo(cesmat//'.CESD', 'L', icesd)
    call jeveuo(cesmat//'.CESL', 'L', icesl)
    call jeveuo(cesmat//'.CESV', 'L', icesv)
!
!
! CONSTRUCTION DU VECTEUR : CONTRAINTE = F(NUMERO D'ORDRE) EN CHAQUE
! POINT DE GAUSS DU PAQUET DE MAILLES.
    l = 1
    nbpg = 0
    nbpgp = 0
    kwork = 0
    sompgw = 0
!
    do 400 imap = nmaini, nmaini+(nbmap-1)
        if (imap .gt. nmaini) then
            kwork = 1
            sompgw = sompgw + zi(jnbpg + imap-2)
        endif
        nbpg = zi(jnbpg + imap-1)
! SI LA MAILLE COURANTE N'A PAS DE POINTS DE GAUSS, LE PROGRAMME
! PASSE DIRECTEMENT A LA MAILLE SUIVANTE.
        if (nbpg .eq. 0) then
            goto 400
        endif
!
        nbpgp = nbpgp + nbpg
        if ((l*int(nbpgt/10.0d0)) .lt. nbpgp) then
            write(6,*)numpaq,'   ',(nbpgp-nbpg)
            l = l + 1
        endif
!
! RECUPERATION DU NOM DU MATERIAU AFFECTE A LA MAILLE COURANTE
! ET DES PARAMETRES ASSOCIES AU CRITERE CHOISI POUR LA MAILLE COURANTE.
!
        optio = 'DOMA_ELGA'
        call rnomat(icesd, icesl, icesv, imap, nomcri,&
                    ibid, ibid, ibid, optio, vala,&
                    valb, coefpa, nommat)
!
        call rcpare(nommat, 'FATIGUE', 'WOHLER', icodwo)
        if (icodwo .eq. 1) then
            call utmess('F', 'FATIGUE1_90', sk=nomcri(1:16))
        endif
!
!
        do 420 ipg = 1, nbpg
!
!            call jerazo('&&DELTAU.VECTPG', tneces, 1)
!
! REMPACER PAR ACMATA
            call acgrdo(nbordr, ordini,&
                        kwork, sompgw, jrwork, tspaq, ipg,&
                        nommet, nommat,&
                        nomcri, vala, coefpa, nomfor, grdvie,&
                        forvie, forcri, valpar, vresu2)
!
!
! C AFFECTATION DES RESULTATS DANS UN CHAM_ELEM SIMPLE
!
            do 550 icmp = 1, 24
                call cesexi('C', jcerd, jcerl, imap, ipg,&
                            1, icmp, jad)
!
!              -- TOUTES LES MAILLES NE SAVENT PAS CALCULER LA FATIGUE :
                if (jad .eq. 0) then
                    ASSERT(icmp.eq.1)
                    ASSERT(ipg.eq.1)
                    goto 400
                endif
                jad = abs(jad)
                zl(jcerl - 1 + jad) = .true.
                zr(jcerv - 1 + jad) = vresu2(icmp)
!
550          continue
!
420      continue
400  end do
!
! MENAGE
!
     call detrsd('CHAM_ELEM_S', cesmat)
! !
!
    call jedema()
end subroutine
