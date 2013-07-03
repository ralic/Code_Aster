subroutine lrmmma(fid, nomamd, nbmail, nbnoma, nbtyp,&
                  typgeo, nomtyp, nnotyp, renumd, nmatyp,&
                  nommai, connex, typmai, prefix, infmed,&
                  modnum, numnoa)
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE DU MAILLAGE -  FORMAT MED - LES MAILLES
!     -    -     -                  -         --
!-----------------------------------------------------------------------
!     ENTREES :
!       FID    : IDENTIFIANT DU FICHIER MED
!       NOMAMD : NOM DU MAILLAGE MED
!       NBMAIL : NOMBRE DE MAILLES DU MAILLAGE
!       NBNOMA : NOMBRE CUMULE DE NOEUDS PAR MAILLE
!       NBTYP  : NOMBRE DE TYPES POSSIBLES POUR MED
!       TYPGEO : TYPE MED POUR CHAQUE MAILLE
!       NNOTYP : NOMBRE DE NOEUDS POUR CHAQUE TYPE DE MAILLES
!       NOMTYP : NOM DES TYPES POUR CHAQUE MAILLE
!       RENUMD : RENUMEROTATION DES TYPES ENTRE MED ET ASTER
!       NMATYP : NOMBRE DE MAILLES PAR TYPE
!       PREFIX : PREFIXE POUR LES TABLEAUX DES RENUMEROTATIONS
!                A UTILISER PLUS TARD
!       MODNUM : INDICATEUR SI LA SPECIFICATION DE NUMEROTATION DES
!                NOEUDS DES MAILLES EST DIFFERENTES ENTRE ASTER ET MED:
!                     MODNUM = 0 : NUMEROTATION IDENTIQUE
!                     MODNUM = 1 : NUMEROTATION DIFFERENTE
!       NUMNOA : TABLEAU DE CORRESPONDANCE DES NOEUDS MED/ASTER.
!                NUMNOA(ITYP,J) : NUMERO DANS MED DU J IEME NOEUD
!                DE LA MAILLE DE TYPE ITYP DE ASTER
!
!     SORTIES:
!       NOMMAI : NOM DES MAILLES
!       CONNEX : CONNECTIVITE
!       TYPMAI : TYPE DES MAILLES
!-----------------------------------------------------------------------
!
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/codlet.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/as_mmhcyr.h"
#include "asterfort/as_mmhear.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    integer :: ntymax
    parameter (ntymax = 69)
!
! 0.1. ==> ARGUMENTS
!
    integer :: fid
    integer :: nbmail, nbnoma
    integer :: nbtyp
    integer :: nmatyp(ntymax), numnoa(ntymax, *), modnum(ntymax)
    integer :: nnotyp(ntymax), typgeo(ntymax)
    integer :: renumd(*)
    integer :: infmed
!
    character(len=6) :: prefix
    character(len=8) :: nomtyp(*)
    character(len=24) :: connex, nommai, typmai
    character(len=*) :: nomamd
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRMMMA' )
!
    integer :: edmail
    parameter (edmail=0)
    integer :: edfuin
    parameter (edfuin=0)
    integer :: ednoda
    parameter (ednoda=0)
!
    integer :: codret
    integer :: ima, idec
    integer :: iaux, jaux
    integer :: imatyp, ityp, letype
    integer :: jcnxma, jmatyp
    integer :: jnomma, jtypma
    integer :: code
    integer :: jnomty(ntymax), jnumty(ntymax), jcxtyp(ntymax)
    integer :: nromai
    integer :: ifm, nivinf
!
    character(len=15) :: saux15
    character(len=8) :: saux08
!
!     ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, nivinf)
!
!====
! 1. ALLOCATIONS
!====
! 1.1. ==> BASE GLOBALE: OBJETS NOMS / CONNECTIVITE / TYPE DES MAILLES
!
    call wkvect(typmai, 'G V I', nbmail, jtypma)
!
! 1.2. ==> CREATION DE PLUSIEURS VECTEURS PAR TYPE DE MAILLE PRESENT :
!              UN VECTEUR CONTENANT LES NOMS DES MAILLES/TYPE
!           +  UN VECTEUR CONTENANT LES NUMEROS DES MAILLES/TYPE
!           +  UN VECTEUR CONTENANT LA CONNECTIVITE DES MAILLE/TYPE
!              (CONNECTIVITE = NOEUDS + UNE VALEUR BIDON(0) SI BESOIN)
!
    call wkvect('&&'//nompro//'.NOMMAI', 'V V K8', nbmail, jnomma)
    call wkvect('&&'//nompro//'.IMATYP', 'V V I', nbmail*2, jmatyp)
!
    do 12 , ityp = 1 , ntymax
!
    if (nmatyp(ityp) .ne. 0) then
!
        call wkvect('&&'//nompro//'.NOM.'//nomtyp(ityp), 'V V K16', nmatyp(ityp), jnomty(ityp))
        call wkvect('&&'//prefix//'.NUM.'//nomtyp(ityp), 'V V I', nmatyp(ityp), jnumty(ityp))
        iaux = nmatyp(ityp) * nnotyp(ityp)
        call wkvect('&&'//nompro//'.CNX.'//nomtyp(ityp), 'V V I', iaux, jcxtyp(ityp))
!
    endif
!
    12 end do
!
!====
! 2. LECTURE
!    ON PARCOURT TOUS LES TYPES POSSIBLES POUR MED ET ON DECLENCHE LES
!    LECTURES SI DES MAILLES DE CE TYPE SONT PRESENTES DANS LE MAILLAGE
!    REMARQUE : GRACE A LA RENUMEROTATION, ON PARCOURT LES TYPES DE
!    MAILLES DANS L'ORDRE CROISSANT DE LEUR TYPE MED. CE N'EST PAS
!    OBLIGATOIRE SI ON DISPOSE DES TABLEAUX DE NUMEROTATION DES MAILLES.
!    MAIS QUAND CES TABLEAUX SONT ABSENTS, LES CONVENTIONS MED PRECISENT
!    QUE LA NUMEROTATION IMPLICITE SE FAIT DANS CET ORDRE. DONC ON
!    LE FAIT !
!====
!
    nromai = 1
    do 21 , letype = 1 , nbtyp
!
! 2.0. ==> PASSAGE DU NUMERO DE TYPE MED AU NUMERO DE TYPE ASTER
!
    ityp = renumd(letype)
!
    if (infmed .ge. 3) then
        write (ifm,2001) nomtyp(ityp), nmatyp(ityp)
    endif
    2001 format('TYPE ',a8,' : ',i10,' MAILLES')
!
    if (nmatyp(ityp) .ne. 0) then
!
! 2.1. ==> LE NUMERO DES MAILLES
!          ON NE TIENT PAS COMPTE DE LA NUMEROTATION DES MAILLES
!          PRESENTE DANS LE FICHIER MED: ON NUMEROTE LES MAILLES
!          SELON LEUR ORDRE D'APPARITION.
!
        do 211 , jaux = 1 , nmatyp(ityp)
        zi(jnumty(ityp)+jaux-1) = nromai
        nromai = nromai + 1
211      continue
!
!
! 2.2. ==> LE NOM DES MAILLES
!          SI LE FICHIER NE CONTIENT PAS DE NOMMAGE DES MAILLES, ON LEUR
!          DONNE UN NOM PAR DEFAUT FORME AVEC LE PREFIXE 'M' SUIVI DE
!          LEUR NUMERO
!
        call as_mmhear(fid, nomamd, zk16(jnomty(ityp)), edmail, typgeo(ityp),&
                    codret)
!
        if (codret .ne. 0) then
            if (infmed .ge. 3) then
                call u2mesk('I', 'MED_19', 1, nomtyp(ityp))
            endif
            if (nbmail .ge. 10000000) then
!           + DE 10 MILLIONS DE MAILLES (AU TOTAL), ON PASSE EN BASE 36
                do 221 , iaux = 1, nmatyp(ityp)
                code = zi(jnumty(ityp)+iaux-1)
                call codlet(code, 'G', saux15)
                zk16(jnomty(ityp)+iaux-1) = 'M'//saux15
221              continue
            else
!           MOINS DE 10 MILLIONS DE MAILLES, ON RESTE EN BASE 10
                do 222 , iaux = 1, nmatyp(ityp)
                code = zi(jnumty(ityp)+iaux-1)
                call codent(code, 'G', saux15)
                zk16(jnomty(ityp)+iaux-1) = 'M'//saux15
222              continue
            endif
            codret = 0
        endif
!
! 2.3. ==> LES CONNECTIVITES
!          LA CONNECTIVITE EST FOURNIE EN STOCKANT TOUS LES NOEUDS A
!          LA SUITE POUR UNE MAILLE DONNEE.
!          C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!
        call as_mmhcyr(fid, nomamd, zi(jcxtyp(ityp)), nmatyp(ityp) * nnotyp(ityp), edfuin,&
                    edmail, typgeo(ityp), ednoda, codret)
        if (codret .ne. 0) then
            saux08='mmhcyr'
            call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                        codret, 0, 0.d0)
        endif
!
        do 231 , imatyp = 1 , nmatyp(ityp)
        ima = zi(jnumty(ityp)+imatyp-1)
        zk8(jnomma+ima-1) = zk16(jnomty(ityp)+imatyp-1)(1:8)
        zi (jtypma+ima-1) = ityp
        idec = (ima-1)*2
        zi(jmatyp+idec) = ityp
        zi(jmatyp+idec+1) = imatyp
231      continue
!
    endif
!
    21 end do
!
!====
! 3. CREATION OBJET CONNECTIVITE DES MAILLES DANS LE BON ORDRE
!====
!
    call jecrec(connex, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmail)
    call jeecra(connex, 'LONT', nbnoma, ' ')
!
    do 31 , ima = 1 , nbmail
!
    idec = (ima-1)*2
    ityp = zi(jmatyp+idec)
    call jeecra(jexnum(connex, ima), 'LONMAX', nnotyp(ityp), ' ')
    call jeveuo(jexnum(connex, ima), 'E', jcnxma)
!
    imatyp = zi(jmatyp+idec+1)
    idec = (imatyp-1)*nnotyp(ityp)
!
!       POUR LES TYPES DE MAILLE DONT LA NUMEROTATION DES NOEUDS
!       ENTRE ASTER ET MED EST IDENTIQUE:
    if (modnum(ityp) .eq. 0) then
        do 311 , jaux = 1 , nnotyp(ityp)
        zi(jcnxma+jaux-1) = zi(jcxtyp(ityp)+jaux-1+idec)
311      continue
!
!       SINON (CF LRMTYP POUR CONNAITRE LA CORRESPONDANCE DES
!       NOEUDS LOCAUX ENTRE ASTER ET MED)
    else
        do 312 , jaux = 1 , nnotyp(ityp)
        zi(jcnxma+jaux-1) = zi(jcxtyp(ityp)+numnoa(ityp,jaux)- 1+idec)
312      continue
    endif
!
    31 end do
!
!====
! 4. CREATION OBJET NOMS DES MAILLES DANS LE BON ORDRE
!====
!
    call jecreo(nommai, 'G N K8')
    call jeecra(nommai, 'NOMMAX', nbmail, ' ')
!
    do 41 , iaux = 1 , nbmail
    call jecroc(jexnom(nommai, zk8(jnomma+iaux-1)))
    41 end do
!
!====
! 5. LA FIN
!====
!
    call jedetr('&&'//nompro//'.NOMMAI')
    call jedetr('&&'//nompro//'.IMATYP')
    do 5 , ityp = 1 , ntymax
    if (nmatyp(ityp) .ne. 0) then
        call jedetr('&&'//nompro//'.NOM.'//nomtyp(ityp))
        call jedetr('&&'//nompro//'.CNX.'//nomtyp(ityp))
    endif
    5 end do
!
    call jedema()
!
end subroutine
