subroutine promor(nuz, base)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infbav.h"
#include "asterfort/infmue.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/moinip.h"
#include "asterfort/moinsr.h"
#include "asterfort/nbec.h"
#include "asterfort/teattr.h"
#include "asterfort/utmess.h"
#include "asterfort/uttrii.h"
#include "asterfort/voiuti.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: nuz
    character(len=1) :: base
!     ------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DE LA STRUCTURE COMPACTE D'UNE MATRICE
!     ------------------------------------------------------------------
! IN  K*14 NU     : NOM DE LA SD_UME_DDL A COMPLETER.
! OUT K*14 NU     : L'OBJET NU EST COMPLETE DES OBJETS .SMOS.XXXX
! IN  K1  BASE    : BASE DE CREATION DU STOCKAGE
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    character(len=8) :: ma, mo, exiele
!----------------------------------------------------------------------
    character(len=14) :: nu
    aster_logical :: ldist, ldgrel, lmadis
    character(len=19) :: nomlig
    integer :: iconx2, ili, iel
    integer :: idprn1
    integer :: idprn2, ifm, niv, iret, nnoe, jnueq
    integer :: vali(3), neqx, iilib, igr, numa, k1, n1, iad1, nddl1
    integer :: iddl, jddl, iamail, jsmhc, ncoef, jsmde, igd, nbss
    integer :: iadequ, nlili, nequ, iimax, jnoip, jsuiv, mxddlt
    integer :: ima, nddlt, jalm, jsmdi, nel, nec, nbsma
    integer ::  rang, imd, jsmh1
!
    character(len=8) :: partit
    real(kind=8) :: valr(2), rcoef, requ
    integer :: nbproc
    integer, pointer :: maille(:) => null()
    integer, pointer :: adne(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: adli(:) => null()
    character(len=24), pointer :: prtk(:) => null()
    integer, pointer :: sssa(:) => null()
    mpi_int :: mrank, msize
!
!-----------------------------------------------------------------------
!     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
!     S.D. MANIPULEES DANS LE SOUS PROGRAMME
!-----------------------------------------------------------------------
!---- FONCTION D ACCES AU CHAMP CONNEX DE LA S.D. MAILLA DE TYPE
!     MAILLAGE
!     ZZCONX(IMAIL,J) = NUMERO DANS LA NUMEROTATION DU MAILLAGE
!         DU NOEUD J DE LA MAILLE IMAIL
#define zzconx(imail,j) connex(zi(iconx2+imail-1)+j-1)
!
!---- NBRE DE NOEUDS DE LA MAILLE IMAIL DU MAILLAGE
!
#define zznbne(imail) zi(iconx2+imail)-zi(iconx2+imail-1)
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS LIEL DES S.D. LIGREL
!     REPERTORIEES DANS LE REPERTOIRE TEMPORAIRE .MATAS.LILI
!     ZZLIEL(ILI,IGREL,J) =
!      SI LA JIEME MAILLE DU LIEL IGREL DU LIGREL ILI EST:
!          -UNE MAILLE DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
!          -UNE MAILLE TARDIVE : -POINTEUR DANS LE CHAMP .NEMA
!
#define zzliel(ili,igrel,j) zi(adli(1+3*(ili-1)+1)-1+ \
    zi(adli(1+3*(ili-1)+2)+igrel-1)+j-1)
!
!---- NBRE DE GROUPES D'ELEMENTS (DE LIEL) DU LIGREL ILI
!
#define zzngel(ili) adli(1+3*(ili-1))
!
!---- NBRE DE NOEUDS DE LA MAILLE TARDIVE IEL ( .NEMA(IEL))
!     DU LIGREL ILI REPERTOIRE .LILI
!     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).NEMA(IEL) )
!
#define zznsup(ili,iel) zi(adne(1+3*(ili-1)+2)+iel)- \
    zi(adne(1+3*(ili-1)+2)+iel-1)-1
!
!---- NBRE D ELEMENTS DU LIEL IGREL DU LIGREL ILI DU REPERTOIRE TEMP.
!     .MATAS.LILI(DIM DU VECTEUR D'ENTIERS .LILI(ILI).LIEL(IGREL) )
!
#define zznelg(ili,igrel) zi(adli(1+3*(ili-1)+2)+igrel)- \
    zi(adli(1+3*(ili-1)+2)+igrel-1)-1
!
!---- NBRE D ELEMENTS SUPPLEMENTAIRE (.NEMA) DU LIGREL ILI DU
!     REPERTOIRE TEMPORAIRE .MATAS.LILI
!
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS NEMA DES S.D. LIGREL
!     REPERTORIEES DANS LE REPERTOIRE TEMPO. .MATAS.LILI
!
#define zznema(ili,iel,j) zi(adne(1+3*(ili-1)+1)-1+ \
    zi(adne(1+3*(ili-1)+2)+iel-1)+j-1)
!
!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS PRNO DES S.D. LIGREL
!     REPERTORIEES DANS NU.LILI DE LA S.D. NUME_DDL ET A LEURS ADRESSES
!     ZZPRNO(ILI,NUNOEL,1) = NUMERO DE L'EQUATION ASSOCIEES AU 1ER DDL
!                            DU NOEUD NUNOEL DANS LA NUMEROTATION LOCALE
!                            AU LIGREL ILI DE .LILI
!     ZZPRNO(ILI,NUNOEL,2) = NOMBRE DE DDL PORTES PAR LE NOEUD NUNOEL
!     ZZPRNO(ILI,NUNOEL,2+1) = 1ER CODE
!     ZZPRNO(ILI,NUNOEL,2+NEC) = NEC IEME CODE
!
#define zzprno(ili,nunoel,l) zi(idprn1-1+zi(idprn2+ili-1)+ \
    (nunoel-1)*(nec+2)+l-1)
!----------------------------------------------------------------------
!
    call infniv(ifm, niv)
    call jemarq()
    nu=nuz
!
!
    call dismoi('NOM_MODELE', nu, 'NUME_DDL', repk=mo)
    call dismoi('NUM_GD_SI', nu, 'NUME_DDL', repi=igd)
    call dismoi('NOM_MAILLA', nu, 'NUME_DDL', repk=ma)
!
!---- QUEL TYPE DE PARTITION ?
!     LDIST=.TRUE.  : LES CALCULS ELEMENTAIRES SONT DISTRIBUES
!     LDGREL=.TRUE. : PARTITION DE TYPE 'GROUP_ELEM'
    call dismoi('NOM_LIGREL', mo, 'MODELE', repk=nomlig)
    call dismoi('PARTITION', nomlig, 'LIGREL', repk=partit)
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
    ldist=.false.
    ldgrel=.false.
    if (partit .ne. ' ') then
        ldist=.true.
        call jeveuo(partit//'.PRTK', 'L', vk24=prtk)
        ldgrel=prtk(1)(1:10).eq.'GROUP_ELEM'
        if (.not.ldgrel) then
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', vi=maille)
        endif
    endif
!
    call jeexin(ma//'.CONNEX', iret)
    if (iret .gt. 0) then
        call jeveuo(ma//'.CONNEX', 'L', vi=connex)
        call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', iconx2)
    endif
!
    if (mo .eq. ' ') then
        nbss=0
    else
        call dismoi('NB_SS_ACTI', mo, 'MODELE', repi=nbss)
        if (nbss .gt. 0) then
            call dismoi('NB_SM_MAILLA', mo, 'MODELE', repi=nbsma)
            call jeveuo(mo//'.MODELE    .SSSA', 'L', vi=sssa)
        endif
    endif
!
!
    call jeveuo(nu//'     .ADNE', 'E', vi=adne)
    call jeveuo(nu//'     .ADLI', 'E', vi=adli)
!
!     -- CAS MATR_DISTRIBUE='OUI' => LMADIS=.TRUE.
    call jeexin(nu//'.NUML.DELG', imd)
    lmadis=(imd.ne.0)
    if (.not.lmadis) then
        call jeveuo(nu//'.NUME.NEQU', 'L', iadequ)
        call jelira(nu//'.NUME.PRNO', 'NMAXOC', nlili)
        call jeveuo(nu//'.NUME.PRNO', 'L', idprn1)
        call jeveuo(jexatr(nu//'.NUME.PRNO', 'LONCUM'), 'L', idprn2)
        call jeveuo(nu//'.NUME.NUEQ', 'L', jnueq)
    else
        call jeveuo(nu//'.NUML.NEQU', 'L', iadequ)
        call jelira(nu//'.NUME.PRNO', 'NMAXOC', nlili)
        call jeveuo(nu//'.NUML.PRNO', 'L', idprn1)
        call jeveuo(jexatr(nu//'.NUML.PRNO', 'LONCUM'), 'L', idprn2)
        call jeveuo(nu//'.NUML.NUEQ', 'L', jnueq)
    endif
!
    nec=nbec(igd)
    nequ=zi(iadequ)
!
!
!
!---- CREATION DE 2 TABLEAUX DE TRAVAIL :
!      .NOIP   :   TABLE DES NUMEROS DE LIGNE
!      .NOSUIV :   TABLE DES CHAINAGES DE LA STRUCTURE CHAINEE
!                  QUI EST CONTRUITE AVANT D'OBTENIR LA
!                  STRUCTURE COMPACTE (SMDI,SMHC) DE LA MATRICE
!     CES 2 OBJETS SONT AGRANDIS SI NECESSAIRE DANS MOINSR.F
!     ON COMMENCE AVEC IIMAX=10
    iimax=10
    call wkvect('&&PROMOR.NOIP', 'V V I', iimax, jnoip)
    call wkvect('&&PROMOR.NOSUIV', 'V V I', iimax, jsuiv)
!
!
!
!     -- ALLOCATION DU VECTEUR &&PROMOR.ANCIEN.LM
!     CE VECTEUR SERA AGRANDI SI NECESSAIRE
    mxddlt=100
    call wkvect('&&PROMOR.ANCIEN.LM', 'V V I', mxddlt, jalm)
!
!
!
!
!     -- ALLOCATION DE .SMOS.SMDI
    call wkvect(nu//'.SMOS.SMDI', base//' V I', nequ, jsmdi)
!
!
!---- INITIALISATION DES TABLEAUX POUR LE STOCKAGE MORSE
!     ATTENTION:   PENDANT LA CONSTRUCTION DE LA STRUCTURE CHAINEE
!                  (SMDI,SMHC,ISUIV) DE LA MATRICE ON A
!     ZI(JSMDI+.): POINTEUR DEBUT DE CHAINE
!     ZI(JSMHC-1+II) : MAILLON QUI CONTIENT L'INDICE COLONNE
!                       DE LA CHAINE II
!     ISUIV(II)     : MAILLON SUIVANT DE LA MEME CHAINE.
!
!     NEQX   : COMPTEUR DU NOMBRE D'EQUATION (CONTROLE)
    neqx=0
!
!     IILIB  : 1-ERE PLACE LIBRE
    iilib=1
!
!
!     -- BOUCLE SUR LES LIGREL DE NU//'.NUME.LILI' :
!     -----------------------------------------------
    do ili = 2, nlili
        call jenuno(jexnum(nu//'.NUME.LILI', ili), nomlig)
        call dismoi('EXI_ELEM', nomlig, 'LIGREL', repk=exiele)
!
        if (nomlig(1:8) .eq. mo) then
            call dismoi('NB_SS_ACTI', mo, 'MODELE', repi=nbss)
        else
            nbss=0
        endif
        if (exiele .eq. 'NON') goto 90
!
!
!       1. TRAITEMENT DES ELEMENTS FINIS CLASSIQUES:
!       --------------------------------------------
        do igr = 1, zzngel(ili)
            if (lmadis) then
                if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 80
            endif
            nel=zznelg(ili,igr)
            do iel = 1, nel
                nddlt=0
                numa=zzliel(ili,igr,iel)
!
                if (numa .gt. 0) then
!                   -- MAILLES DU MAILLAGE :
                    if (lmadis .and. ldist .and. .not.ldgrel) then
                        if (maille(numa) .ne. rang) goto 70
                    endif
!
                    nnoe=zznbne(numa)
                    do k1 = 1, nnoe
                        n1=zzconx(numa,k1)
                        iad1=zzprno(1,n1,1)
                        nddl1=zzprno(1,n1,2)
                        if (mxddlt .lt. (nddlt+nddl1)) then
                            mxddlt=2*(nddlt+nddl1)
                            call juveca('&&PROMOR.ANCIEN.LM', mxddlt)
                            call jeveuo('&&PROMOR.ANCIEN.LM', 'E', jalm)
                        endif
                        do iddl = 1, nddl1
                            zi(jalm+nddlt+iddl-1)=zi(jnueq-1+iad1+&
                            iddl-1)
                        end do
                        nddlt=nddlt+nddl1
                    end do
!
                else
!                   -- MAILLES TARDIVES :
                    if (lmadis .and. ldist .and. .not.ldgrel) then
                        if (rang .ne. 0) goto 70
                    endif
!
                    numa=-numa
                    nnoe=zznsup(ili,numa)
                    do k1 = 1, nnoe
                        n1=zznema(ili,numa,k1)
                        if (n1 .lt. 0) then
                            n1=-n1
                            iad1=zzprno(ili,n1,1)
                            nddl1=zzprno(ili,n1,2)
                        else
                            iad1=zzprno(1,n1,1)
                            nddl1=zzprno(1,n1,2)
                        endif
                        if (mxddlt .lt. (nddlt+nddl1)) then
                            mxddlt=2*(nddlt+nddl1)
                            call juveca('&&PROMOR.ANCIEN.LM', mxddlt)
                            call jeveuo('&&PROMOR.ANCIEN.LM', 'E', jalm)
                        endif
                        do iddl = 1, nddl1
                            zi(jalm+nddlt+iddl-1)=zi(jnueq-1+iad1+&
                            iddl-1)
                        end do
                        nddlt=nddlt+nddl1
                    end do
                endif
!
!       -- TRI EN ORDRE CROISSANT POUR L'INSERTION DES COLONNES
                ASSERT(nddlt.le.mxddlt)
                call uttrii(zi(jalm), nddlt)
!
!       -- INSERTION DES COLONNES DE L'ELEMENT DANS
!           LA STRUCTURE CHAINEE
                do iddl = 0, nddlt-1
                    jddl=jsmdi+zi(jalm+iddl)-1
                    if (zi(jddl) .eq. 0) neqx=neqx+1
                    call moinsr(zi(jalm+iddl), iddl+1, jalm, jsmdi, jsuiv,&
                                '&&PROMOR.NOSUIV', jnoip, '&&PROMOR.NOIP', iilib, iimax)
                end do
 70             continue
            end do
 80         continue
        end do
!
!
!
!       3. TRAITEMENT DES SOUS-STRUCTURES STATIQUES :
!       ---------------------------------------------
 90     continue
        if (nbss .gt. 0) then
            do ima = 1, nbsma
                if (sssa(ima) .eq. 0) goto 130
                nddlt=0
                call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', iamail)
                call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nnoe)
                do k1 = 1, nnoe
                    n1=zi(iamail-1+k1)
                    ASSERT(n1.ne.0)
                    iad1=zzprno(1,n1,1)
                    nddl1=zzprno(1,n1,2)
                    if (mxddlt .lt. (nddlt+nddl1)) then
                        mxddlt=2*(nddlt+nddl1)
                        call juveca('&&PROMOR.ANCIEN.LM', mxddlt)
                        call jeveuo('&&PROMOR.ANCIEN.LM', 'E', jalm)
                    endif
                    do iddl = 1, nddl1
                        zi(jalm+nddlt+iddl-1)=zi(jnueq-1+iad1+iddl-1)
                    end do
                    nddlt=nddlt+nddl1
                end do
!
                ASSERT(nddlt.le.mxddlt)
                call uttrii(zi(jalm), nddlt)
                do iddl = 0, nddlt-1
                    jddl=jsmdi+zi(jalm+iddl)-1
                    if (zi(jddl) .eq. 0) neqx=neqx+1
                    call moinsr(zi(jalm+iddl), iddl+1, jalm, jsmdi, jsuiv,&
                                '&&PROMOR.NOSUIV', jnoip, '&&PROMOR.NOIP', iilib, iimax)
                end do
130             continue
            end do
        endif
    end do
!
    if ((neqx.ne.nequ) .and. (.not.lmadis)) then
        vali(1)=nequ
        vali(2)=neqx
        call utmess('F', 'ASSEMBLA_65', ni=2, vali=vali)
    endif
!
!
!
!     DESIMBRIQUATION DE CHAINES POUR OBTENIR LA STRUCTURE COMPACTE
!     (SMDI,SMHC) DE LA MATRICE
    call wkvect(nu//'.SMOS.SMH1', base//' V S', iimax, jsmh1)
    call moinip(neqx, ncoef, zi(jsmdi), zi(jsuiv), zi(jnoip),&
                zi4(jsmh1))
    call wkvect(nu//'.SMOS.SMHC', base//' V S', ncoef, jsmhc)
    do iddl = 1, ncoef
        zi4(jsmhc+iddl-1)=zi4(jsmh1+iddl-1)
    end do
    call jedetr(nu//'.SMOS.SMH1')
!
!
!     -- CREATION ET REMPLISSAGE DE .SMDE
    call wkvect(nu//'.SMOS.SMDE', base//' V I', 6, jsmde)
    zi(jsmde-1+1)=nequ
    zi(jsmde-1+2)=ncoef
    zi(jsmde-1+3)=1
!
!
    call jedetr('&&PROMOR.NOIP')
    call jedetr('&&PROMOR.NOSUIV')
    call jedetr('&&PROMOR.ANCIEN.LM   ')
!
    if (niv .ge. 1) then
        vali(1) = nequ
        vali(2) = ncoef
        vali(3) = 2*ncoef-nequ
        rcoef = ncoef
        requ = nequ
        valr(1) = (100.d0*(2.d0*rcoef-requ)) / (requ*requ)
        call utmess('I', 'FACTOR_2', ni=3, vali=vali, sr=valr(1))
    endif
!
    call jedema()
!
end subroutine
