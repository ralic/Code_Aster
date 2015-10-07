subroutine caimch(chargz)
!
implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/aflrch.h"
#include "asterfort/afrela.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
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
!
    character(len=*) :: chargz
!
!       CAIMCH -- TRAITEMENT DU MOT FACTEUR CHAMNO_IMPO
!
!      TRAITEMENT DU MOT FACTEUR CHAMNO_IMPO DE AFFE_CHAR_MECA
!      CE MOT FACTEUR PERMET D'IMPOSER SUR DES DDL DES NOEUDS
!      D'UN MODELELES VALEURS DES COMPOSANTES DU CHAM_NO DONNE
!      APRES LE MOT CLE : CHAM_NO.
!
! -------------------------------------------------------
!  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -   LA  CHARGE EST ENRICHIE
!                                   DE LA RELATION LINEAIRE DECRITE
!                                   CI-DESSUS.
! -------------------------------------------------------
!
    character(len=2) :: typlag
    character(len=4) :: tych, typval, typcoe
    character(len=8) :: chamno, noma, nomcmp, nomnoe, betaf
    character(len=8) :: charge, poslag, nomgd
    character(len=16) :: motfac
    character(len=19) :: lisrel, cham19, prchno
    character(len=24) :: noeuma
    real(kind=8) :: beta, alpha
    complex(kind=8) :: betac
    integer :: ibid, idcoec, idcoer, idddl,  idimen, idirec
    integer :: idnoeu,  iequa, ino, inocmp, iocc
    integer :: iret, k, nb, nbcmp, nbec, nbnoeu, nbterm
    integer :: nequa, nliai, nucmp
    real(kind=8) :: vale, zero
    integer, pointer :: deeq(:) => null()
    real(kind=8), pointer :: vvale(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    motfac = 'CHAMNO_IMPO'
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 30
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
! --- ALPHA EST LE COEFFICIENT REEL DE LA
! --- RELATION LINEAIRE
!
    alpha = 1.0d0
! --- BETA, BETAC ET BETAF SONT LES VALEURS DU SECOND MEMBRE DE LA
! --- RELATION LINEAIRE SUIVANT QUE C'EST UN REEL, UN COMPLEXE OU
! --- UNE FONCTION, DANS NOTRE CAS C'EST UN REEL
!
    beta = zero
    betac = (0.0d0,0.0d0)
    betaf = '&FOZERO'
!
    cham19 = '                   '
    charge = chargz
!
! --- TYPE DES VALEURS AU SECOND MEMBRE DE LA RELATION
!
    typval = 'REEL'
!
! --- TYPE DES VALEURS DES COEFFICIENTS
!
    typcoe = 'REEL'
!
! --- NOM DE LA LISTE_RELA
!
    lisrel = '&CAIMCH.RLLISTE'
!
! --- BOUCLE SUR LES OCCURENCES DU MOT-FACTEUR CHAMNO_IMPO :
!     -------------------------------------------------------
    do iocc = 1, nliai
!
! ---   ON REGARDE SI LES MULTIPLICATEURS DE LAGRANGE SONT A METTRE
! ---   APRES LES NOEUDS PHYSIQUES LIES PAR LA RELATION DANS LA MATRICE
! ---   ASSEMBLEE :
! ---   SI OUI TYPLAG = '22'
! ---   SI NON TYPLAG = '12'
!
        call getvtx(motfac, 'NUME_LAGR', iocc=iocc, scal=poslag, nbret=ibid)
        if (poslag .eq. 'APRES') then
            typlag = '22'
        else
            typlag = '12'
        endif
!
! ---   RECUPERATION DU CHAMNO
!       ----------------------
        call getvid(motfac, 'CHAM_NO', iocc=iocc, scal=chamno, nbret=nb)
        if (nb .eq. 0) then
            call utmess('F', 'MODELISA2_83')
        endif
!
        cham19(1:8) = chamno
!
! ---   VERIFICATION DE L'EXISTENCE DU CHAMNO
!       -------------------------------------
        call jeexin(cham19//'.VALE', iret)
        if (iret .eq. 0) then
            call utmess('F', 'MODELISA2_84')
        endif
!
! ---   VERIFICATION DU TYPE DU CHAMP
!       -----------------------------
        call dismoi('TYPE_CHAMP', chamno, 'CHAM_NO', repk=tych)
!
        if (tych .ne. 'NOEU') then
            call utmess('F', 'MODELISA2_85')
        endif
!
! ---   RECUPERATION DE LA VALEUR DU SECOND MEMBRE DE LA RELATION
! ---   LINEAIRE
!       --------
        call getvr8(motfac, 'COEF_MULT', iocc=iocc, scal=alpha, nbret=nb)
        if (nb .eq. 0) then
            call utmess('F', 'MODELISA2_86')
        endif
!
! ---   RECUPERATION DE LA GRANDEUR ASSOCIEE AU CHAMNO :
!       ----------------------------------------------
        call dismoi('NOM_GD', chamno, 'CHAM_NO', repk=nomgd)
!
! ---   RECUPERATION DU NOMBRE DE MOTS SUR-LESQUELS SONT CODEES LES
! ---   LES INCONNUES ASSOCIEES A LA GRANDEUR DE NOM NOMGD
!       --------------------------------------------------
        call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbec)
        if (nbec .gt. 10) then
            call utmess('F', 'MODELISA2_87', sk=nomgd)
        endif
!
! ---   RECUPERATION DU MAILLAGE ASSOCIE AU CHAM_NO
!       -------------------------------------------
        call dismoi('NOM_MAILLA', chamno, 'CHAM_NO', repk=noma)
!
! ---   RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
!       --------------------------------------------
        call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoeu)
!
! ---   RECUPERATION DU NOMBRE DE TERMES DU CHAM_NO
!       -------------------------------------------
        call dismoi('NB_EQUA', chamno, 'CHAM_NO', repi=nequa)
!
! ---   RECUPERATION DU PROF_CHNO DU CHAM_NO
!       ------------------------------------
        call dismoi('PROF_CHNO', chamno, 'CHAM_NO', repk=prchno)
!
! ---   RECUPERATION DU NOMBRE DE COMPOSANTES ASSOCIEES A LA LA GRANDEUR
!       ----------------------------------------------------------------
        call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nbcmp)
!
! ---   RECUPERATION DU NOM DES COMPOSANTES ASSOCIEES A LA LA GRANDEUR
!       --------------------------------------------------------------
        call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', inocmp)
!
! ---   RECUPERATION DU .VALE DU CHAM_NO
!       --------------------------------
        call jeveuo(cham19//'.VALE', 'E', vr=vvale)
!
! ---   RECUPERATION DU .DEEQ DU PROF_CHNO
!       ----------------------------------
        call jeveuo(prchno//'.DEEQ', 'L', vi=deeq)
!
        nbterm = nequa
!
! ---   CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
! ---   DE LA LISTE_RELA
!       ----------------
! ---     VECTEUR DU NOM DES NOEUDS
        call wkvect('&&CAIMCH.LISNO', 'V V K8', nbterm, idnoeu)
! ---     VECTEUR DU NOM DES DDLS
        call wkvect('&&CAIMCH.LISDDL', 'V V K8', nbterm, idddl)
! ---      VECTEUR DES COEFFICIENTS REELS
        call wkvect('&&CAIMCH.COER', 'V V R', nbterm, idcoer)
! ---     VECTEUR DES COEFFICIENTS COMPLEXES
        call wkvect('&&CAIMCH.COEC', 'V V C', nbterm, idcoec)
! ---     VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
        call wkvect('&&CAIMCH.DIRECT', 'V V R', 3*nbterm, idirec)
! ---     VECTEUR DES DIMENSIONS DE CES DIRECTIONS
        call wkvect('&&CAIMCH.DIME', 'V V I', nbterm, idimen)
!
! ---   COLLECTION DES NOMS DES NOEUDS DU MAILLAGE
!       ------------------------------------------
        noeuma = noma//'.NOMNOE'
!
! ---   AFFECTATION DES TABLEAUX DE TRAVAIL :
!       -----------------------------------
        k = 0
!
! ---   BOUCLE SUR LES TERMES DU CHAM_NO
!
        do iequa = 1, nequa
!
! ---     INO  : NUMERO DU NOEUD INO CORRESPONDANT AU DDL IEQUA
!
            ino = deeq(1+2* (iequa-1)+1-1)
!
! ---     NUCMP  : NUMERO DE COMPOSANTE CORRESPONDANTE AU DDL IEQUA
!
            nucmp = deeq(1+2* (iequa-1)+2-1)
!
! ---     ON NE PREND PAS EN COMPTE LES MULTIPLICATEURS DE LAGRANGE
! ---     (CAS OU NUCMP < 0)
!
            if (nucmp .gt. 0) then
!
! ---       RECUPERATION DU NOM DU NOEUD INO
!
                call jenuno(jexnum(noeuma, ino), nomnoe)
!
                vale = vvale(iequa)
!
                k = k + 1
                nomcmp = zk8(inocmp+nucmp-1)
                zk8(idnoeu+k-1) = nomnoe
                zk8(idddl+k-1) = nomcmp
                zr(idcoer+k-1) = alpha
                beta = vale
!
! ---       AFFECTATION DE LA RELATION A LA LISTE_RELA  :
!
                call afrela(zr(idcoer+k-1), zc(idcoec+k-1), zk8(idddl+k- 1), zk8(idnoeu+k-1),&
                            zi(idimen+k-1), [0.d0], 1, beta, betac,&
                            betaf, typcoe, typval, typlag, 0.d0,&
                            lisrel)
            endif
!
        end do
!
        nbterm = k
!
! ---   MENAGE :
!       ------
        call jedetr('&&CAIMCH.LISNO')
        call jedetr('&&CAIMCH.LISDDL')
        call jedetr('&&CAIMCH.COER')
        call jedetr('&&CAIMCH.COEC')
        call jedetr('&&CAIMCH.DIRECT')
        call jedetr('&&CAIMCH.DIME')
!
    end do
!
! --- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ----------------------------------------
    call aflrch(lisrel, charge, 'LIN')
!
! --- MENAGE :
!     ------
    call jedetr(lisrel)
!
 30 continue
!
    call jedema()
!
end subroutine
