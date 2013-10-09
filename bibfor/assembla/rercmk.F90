subroutine rercmk(nu, mo, ma, nlili, nm,&
                  nl, nbntt)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/indiis.h"
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
#include "asterfort/renuu1.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: mo, ma
    character(len=14) :: nu
    integer :: nlili, nm, nl, nbntt
! ----------------------------------------------------------------------
!     BUT:  CETTE ROUTINE SERT A RENUMEROTER LES NOEUDS D'UN NUME_DDL
!           SUIVANT L'ALGORITHME DE REVERSE-CUTHILL-MAC-KEE.
!     IN:
!     ---
!       NU : NOM DU NUME_DDL QUE L'ON RENUMEROTE
!       MO : NOM DU MODELE SOUS-JACENT AU NUME_DDL
!       MA : NOM DU MAILLAGE SOUS-JACENT AU NUME_DDL
!       NLILI: NOMBRE DE LIGREL DE L'OBJET .LILI
!       NM   : NOMBRE DE NOEUDS PHYSIQUES DU MAILLAGE
!       NL   : NOMBRE DE NOEUDS DE LAGRANGE DU MAILLAGE
!       NBNTT: NOMBRE DE NOEUDS MAXI (NUME_DDL)
!
!     OUT:
!     ----
!
!      ON REMPLIT LES VECTEURS NU//'.NEWN' ET NU//'.OLDN'
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: exiele
    character(len=24) :: nomli2
    character(len=19) :: nomlig
!
!-----------------------------------------------------------------------
    integer :: i, iacoin, iaconx, iaexi1, iagrel, ialcoi, ialiel
    integer :: iamail, ianbco, ianbno, ianema, ianew1, ianewn, iaold1
    integer :: iaoldn, iaordo, iasssa, ico, icol, icumul
    integer :: iel, ifm, igrel, iinew, iino, iio1
    integer :: iio2, ilconx, ili, illiel, ilnema, ima, ino
    integer :: irempl, iret, j, jjno, jno, jrang, k
    integer :: l1, l2, ll1, ll2, longi, longo, n1i
    integer :: n1j, n2i, n2j, nbco, nbcomp, nbel, nbgrel
    integer :: nbi, nbma, nbnm, nbnmre, nbnoma, nbnot, nbntre
    integer :: nbsma, nbssa, newnno, niv
!-----------------------------------------------------------------------
    call jemarq()
!
!-----RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, niv)
!----------------------------------------------------------------------
    nbnoma= nm+nl
!
!     -- ALLOCATION DES OBJETS .NEW1 ET .OLD1  (PROVISOIRES) :
!        CES OBJETS REPRESENTENT LA RENUMEROTATION DE TOUS LES NOEUDS
!     ---------------------------------------------------------------
    call wkvect('&&RERCMK.NEW1', 'V V I', nbntt, ianew1)
    call wkvect('&&RERCMK.OLD1', 'V V I', nbntt, iaold1)
!
!     ---------------------------------------------------------------
!        ON CALCULE LA DIMENSION DE LA TABLE DE CONNECTIVITE INVERSE:
!             (EN FAIT, ON SUR-DIMENSIONNE)
!     ---------------------------------------------------------------
!
!     -- ORDO EST UNE TABLE DE TRAVAIL QUI DOIT POUVOIR CONTENIR
!        UNE LIGNE DE CONNECTIVITE INVERSE.
    call wkvect('&&RERCMK.ORDO', 'V V I', nbntt, iaordo)
!
    call wkvect('&&RERCMK.LCOI', 'V V I', nbntt, ialcoi)
!     -- .LCOI(INO) CONTIENDRA L'ADRESSE DANS .COIN DE LA LISTE
!        DES NOEUDS CONNECTES A INO (C'EST LE VECTEUR CUMULE DE .EXI1)
!        C'EST EN QUELQUE SORTE LE POINTEUR DE LONGUEUR CUMULEE DE .COIN
!
    call wkvect('&&RERCMK.NBCO', 'V V I', nbntt, ianbco)
!     -- .NBCO(INO) CONTIENDRA AU FUR ET A MESURE DE LA CONSTRUCTION
!         DE LA TABLE DE CONNECTIVITE INVERSE, LE NOMBRE REEL DE NOEUDS
!         CONNECTES A INO.
!
!
!
!     -----------------------------------------------------------------
!        RECUPERATION DE .EXI1
!        ALLOCATION DE LA TABLE DE CONNECTIVITE INVERSE: .COIN
!        REMPLISSAGE DU "POINTEUR DE LONGUEUR" .LCOI
!     -----------------------------------------------------------------
!
    call jeveuo(nu//'.EXI1', 'L', iaexi1)
!
    icumul=0
!     -- NBNTRE EST LE NOMBRE TOTAL DE NOEUDS A RENUMEROTER
    nbntre=0
!
    zi(ialcoi-1+1)= 1
    do 5  , ino=1,nbntt-1
    icumul= icumul+zi(iaexi1+ino)
    zi(ialcoi-1+ino+1)= zi(ialcoi-1+ino)+zi(iaexi1+ino)
    if (zi(iaexi1+ino) .gt. 0) nbntre= nbntre+1
    5 end do
!
    icumul= icumul+zi(iaexi1+nbntt)
    if (zi(iaexi1+nbntt) .gt. 0) nbntre= nbntre+1
!
    call wkvect('&&RERCMK.COIN', 'V V I', icumul, iacoin)
!
!
!     -----------------------
!     --REMPLISSAGE DE .COIN:
!     -----------------------
!
    call dismoi('NB_MA_MAILLA', mo, 'MODELE', repi=nbma)
    if (nbma .gt. 0) then
        call jeveuo(ma//'.CONNEX', 'L', iaconx)
        call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', ilconx)
    endif
!
!
!     -- 1ERE ETAPE : (SUPER)MAILLES DU MAILLAGE:
!     -------------------------------------------
    call dismoi('NB_SS_ACTI', mo, 'MODELE', repi=nbssa)
    call dismoi('NB_SM_MAILLA', mo, 'MODELE', repi=nbsma)
    if (nbssa .gt. 0) then
        call jeveuo(mo//'.MODELE    .SSSA', 'L', iasssa)
    else
        goto 12
    endif
!
    do ima = 1, nbsma
        if (zi(iasssa-1+ima) .eq. 1) then
            call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', iamail)
            call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nbnm)
            do i = 1, nbnm
                ino=zi(iamail-1+i)
                iino=ino
                if (ino .le. 0) then
                    call utmess('F', 'ASSEMBLA_36')
                endif
                do j = i+1, nbnm
                    jno=zi(iamail-1+j)
                    jjno=jno
                    jrang= indiis(zi(iacoin+zi(ialcoi-1+iino)-1)&
                    ,jjno,1,zi(ianbco-1+iino))
!
                    if (jrang .eq. 0) then
                        irempl=zi(ianbco-1+iino) +1
                        zi(ianbco-1+iino)=irempl
                        zi(iacoin+zi(ialcoi-1+iino)-1+ irempl-1)=&
                        jjno
!
                        irempl=zi(ianbco-1+jjno) +1
                        zi(ianbco-1+jjno)=irempl
                        zi(iacoin+zi(ialcoi-1+jjno)-1+ irempl-1)=&
                        iino
                    endif
                end do
            end do
        endif
    end do
!
 12 continue
!
!
!     -- 2EME ETAPE : MAILLES TARDIVES (OU NON) DES LIGRELS
!                     (MODELE + LISTE DE CHARGES)
!     -----------------------------------------------------
!
    nbnot=0
    do 30 , ili=2,nlili
    call jenuno(jexnum(nu//'.NUME.LILI', ili), nomli2)
    nomlig=nomli2(1:19)
    call dismoi('EXI_ELEM', nomlig, 'LIGREL', repk=exiele)
    if (exiele(1:3) .eq. 'NON') goto 30
!
    call jeveuo(nomlig//'.LIEL', 'L', ialiel)
    call jeveuo(jexatr(nomlig//'.LIEL', 'LONCUM'), 'L', illiel)
    call jelira(nomlig//'.LIEL', 'NMAXOC', nbgrel)
!
    call jeexin(nomlig//'.NEMA', iret)
    if (iret .gt. 0) then
        call jeveuo(nomlig//'.NEMA', 'L', ianema)
        call jeveuo(jexatr(nomlig//'.NEMA', 'LONCUM'), 'L', ilnema)
    endif
!
    do igrel = 1, nbgrel
        nbel= zi(illiel-1+igrel+1)-zi(illiel-1+igrel) -1
        iagrel= ialiel + zi(illiel-1+igrel) -1
        do iel = 1, nbel
            ima= zi(iagrel -1 +iel)
            if (ima .gt. 0) then
                nbnm= zi(ilconx-1+ima+1)-zi(ilconx-1+ima)
                iamail= iaconx + zi(ilconx-1+ima) -1
            else
                nbnm= zi(ilnema-1-ima+1)-zi(ilnema-1-ima) -1
                iamail = ianema + zi(ilnema-1-ima) -1
            endif
!
            do i = 1, nbnm
                ino=zi(iamail-1+i)
                iino= ino
                if (ino .lt. 0) iino=nbnoma+nbnot-ino
!
                do j = i+1, nbnm
                    jno=zi(iamail-1+j)
                    jjno= jno
                    if (jno .lt. 0) jjno=nbnoma+nbnot-jno
!
                    jrang= indiis(zi(iacoin+zi(ialcoi-1+iino)-1)&
                        ,jjno,1,zi(ianbco-1+iino))
!
                    if (jrang .eq. 0) then
                        irempl=zi(ianbco-1+iino) +1
                        zi(ianbco-1+iino)=irempl
                        zi(iacoin+zi(ialcoi-1+iino)-1+ irempl-1)=&
                            jjno
!
                        irempl=zi(ianbco-1+jjno) +1
                        zi(ianbco-1+jjno)=irempl
                        zi(iacoin+zi(ialcoi-1+jjno)-1+ irempl-1)=&
                            iino
                    endif
                end do
            end do
        end do
    end do
!
    call jeveuo(nomlig//'.NBNO', 'L', ianbno)
    nbnot= nbnot+zi(ianbno)
    30 end do
!
!
!
!     --CALCUL DES OBJETS .NEW1 ET .OLD1 :
!     ------------------------------------
!
    iinew=0
!
!     -- NBCOMP COMPTE LE NOMBRE DE COMPOSANTES CONNEXES DU MODELE
    nbcomp=0
 50 continue
    nbcomp= nbcomp+1
!
!     --ON INITIALISE L'ALGORITHME PAR LE NOEUD I QUI A LA CONNECTIVITE
!     -- LA PLUS FAIBLE (PARMI CEUX RESTANT A RENUMEROTER):
!     "I= MIN(NBCO)"
!     ----------------------------------------------------------------
    i=0
    do k = 1, nbntt
        if (zi(iaexi1+k) .eq. 0) goto 51
        if (zi(ianew1-1+k) .ne. 0) goto 51
        if (i .eq. 0) then
            i=k
        else
            if (zi(ianbco-1+k) .lt. zi(ianbco-1+i)) i=k
        endif
 51     continue
    end do
    ASSERT(i.ne.0)
!
    iinew=iinew+1
    zi(ianew1-1+i)=iinew
    zi(iaold1-1+iinew)=i
!     -- SI ON A RENUMEROTE TOUS LES NOEUDS ATTENDUS, ON SORT :
    if (iinew .eq. nbntre) goto 200
    ico=iinew
!
100 continue
    longi= zi(ianbco-1+i)
    call renuu1(zi(iacoin-1+zi(ialcoi-1+i)), longi, zi(iaordo), longo, zi(ianbco),&
                zi(ianew1))
    do j = 1, longo
        iinew=iinew+1
        zi(ianew1-1+zi(iaordo-1+j))=iinew
        zi(iaold1-1+iinew)=zi(iaordo-1+j)
!        -- SI ON A RENUMEROTE TOUS LES NOEUDS ATTENDUS, ON SORT :
        if (iinew .eq. nbntre) goto 200
    end do
    ico=ico+1
    i=zi(iaold1-1+ico)
    if (i .eq. 0) then
        goto 50
    else
        goto 100
    endif
!
200 continue
!
!     -- ON COMPACTE .OLD1 DANS .NEWN ET .OLDN
!     POUR NE CONSERVER QUE LES NOEUDS PHYSIQUES :
!     --------------------------------------------
    call jeveuo(nu//'.OLDN', 'E', iaoldn)
    call jeveuo(nu//'.NEWN', 'E', ianewn)
!
    icol=0
    do i = 1, nbntt
        iio1 = zi(iaold1-1+i)
        if (iio1 .eq. 0) goto 3
        if (iio1 .gt. nm) then
            icol=icol+1
        else
            iio2=i-icol
            if ((iio1.lt.1) .or. (iio1.gt.nm)) then
                call utmess('F', 'ASSEMBLA_38')
            endif
            if ((iio2.lt.1) .or. (iio2.gt.nm)) then
                call utmess('F', 'ASSEMBLA_38')
            endif
            zi(ianewn-1+iio1)=i-icol
        endif
    end do
  3 continue
!     -- NBNMRE EST LE NOMBRE DE NOEUDS PHYSIQUES A RENUMEROTER
    nbnmre= iio2
!
!     -- ON FINIT EN "REVERSANT" LE TOUT :
!     ------------------------------------
    do i = 1, nm
        if (zi(ianewn-1+i) .eq. 0) goto 300
        newnno = nbnmre+1-zi(ianewn-1+i)
        zi(ianewn-1+i)= newnno
        zi(iaoldn-1+newnno)= i
300     continue
    end do
!
!
!     -- ON ECRIT LES LARGEURS DE BANDE MOYENNES AVANT ET APRES:
!     ----------------------------------------------------------
    if (niv .ge. 1) then
        write(ifm,*) '--- RENUMEROTATION DES NOEUDS DU MODELE (RCMK) :'
        write(ifm,*) '   --- NOMBRE DE COMPOSANTES CONNEXES DU MODELE :'&
     &  ,nbcomp
    endif
!
    nbi=0
    ll1=0
    ll2=0
    do i = 1, nm
        if (zi(iaexi1+i) .eq. 0) goto 600
        nbi= nbi+1
        nbco= zi(ianbco-1+i)
        l1=1
        l2=1
        do j = 1, nbco
            n1i=i
            n1j=zi(iacoin-2+zi(ialcoi-1+i)+j)
            if (n1j .gt. nm) goto 601
            l1= max(l1,(n1i-n1j)+1)
!
            n2i= zi(ianewn-1+n1i)
            n2j= zi(ianewn-1+n1j)
            l2= max(l2,(n2i-n2j)+1)
601         continue
        end do
        ll1=ll1+l1
        ll2=ll2+l2
600     continue
    end do
    if (niv .ge. 1) then
        write(ifm,*)'   --- HAUTEUR DE COLONNE MOYENNE (EN NOEUDS)'
        write(ifm,*)'        (EN NE TENANT COMPTE QUE DES NOEUDS '&
     &                       //'PHYSIQUES)'
        write(ifm,fmt='(A30,1PD10.3)')'        AVANT RENUMEROTATION:',&
     &             dble(ll1)/nbi
        write(ifm,fmt='(A30,1PD10.3)')'        APRES RENUMEROTATION:',&
     &             dble(ll2)/nbi
!
        if (ll1 .le. ll2) then
            write(ifm,*)'   --- LA NOUVELLE NUMEROTATION OBTENUE PAR '&
     &   //'L ALGORITHME "RCMK" NE SEMBLE PAS'
            write(ifm,*)'       MEILLEURE QUE L ORIGINALE. ELLE L''EST'&
     &   //' PEUT ETRE QUAND MEME DU FAIT DE LA '
            write(ifm,*)'       PRISE EN COMPTE DES RELATIONS LINEAIRES'&
     &  // ' ENTRE NOEUDS.'
        endif
    endif
!
    call jedetr('&&RERCMK.NEW1')
    call jedetr('&&RERCMK.OLD1')
    call jedetr('&&RERCMK.ORDO')
    call jedetr('&&RERCMK.LCOI')
    call jedetr('&&RERCMK.NBCO')
    call jedetr('&&RERCMK.COIN')
!
    call jedema()
end subroutine
