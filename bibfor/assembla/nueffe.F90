subroutine nueffe(nb_ligr, list_ligr, base, nume_ddlz, renumz,&
                  solver , modelocz)
!
implicit none
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/creprn.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infbav.h"
#include "asterfort/infmue.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nddl.h"
#include "asterfort/nudeeq.h"
#include "asterfort/nulili.h"
#include "asterfort/nuno1.h"
#include "asterfort/renuno.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
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

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1501
! person_in_charge: jacques.pellet at edf.fr
!
    integer, intent(in) :: nb_ligr
    character(len=24), pointer, intent(in) :: list_ligr(:)
    character(len=2), intent(in) :: base
    character(len=*), intent(in) :: nume_ddlz
    character(len=*), intent(in) :: renumz
    character(len=19),optional, intent(in) :: solver
    character(len=*), optional, intent(in) :: modelocz
!
! --------------------------------------------------------------------------------------------------
!
! Factor
!
! Numbering - Create NUME_EQUA objects
!
! --------------------------------------------------------------------------------------------------
!
! In  nb_ligr        : number of LIGREL in list
! In  list_ligr      : pointer to list of LIGREL
! In  nume_ddl       : name of nume_ddl object
! In  base           : JEVEUX base to create objects
!                      base(1:1) => PROF_CHNO objects
!                      base(2:2) => NUME_DDL objects
! In  renum          : method for renumbering equation
!                       SANS/RCMK/MD/MDA/METIS
! In  solver         : name of solver datastructure
! In  modelocz       : local mode for GRANDEUR numbering
!
!-----------------------------------------------------------------------
! ATTENTION : NE PAS FAIRE JEMARQ/JEDEMA CAR NULILI
!             RECOPIE DES ADRESSES JEVEUX DANS .ADNE ET .ADLI
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: moloc
    character(len=8) :: gran_name, kbid
    integer :: n, igds, nec, nlili
    character(len=8) :: nomcmp
    character(len=8) :: mesh
    character(len=14) :: nume_ddl
    character(len=16) :: nomte
    character(len=24) :: nnli, psuiv, lsuiv, vsuiv, num21, nuno, nomli
    character(len=24) :: derli, num2, dsclag, exi1, newn, oldn
    character(len=19) :: nume_equa
    character(len=24) :: nequ, refn
    character(len=19) :: prof_chno
    character(len=24) :: lili, prno, nueq
    integer :: nb_node_mesh, ilim, itypel, nequa
    integer :: i, iad,   ianueq,  icddlb
    integer :: icer1, icer2, iconx1, iconx2, iddlag, iderli, idlgns
    integer :: idnequ, idnocm, idprn1, idprn2, idref
    integer :: iec, iel, iexi1, ifm, igr, ilag, ilag2, ilag3
    integer :: ili, ilsuiv, inewn, ino, inulag, inum2, inum21
    integer :: inuno1, inuno2, ioldn, iprnm,  ipsuiv, ire, iret
    integer :: ivsuiv, j, j1, jnulag, jprno, k, l, l1, l2, long, n0
    integer :: n0re, n1, n1m1re, n1re, n2, n21, n3, nbcmp, nbn, nb_lagr_mesh
    integer :: nb_node, nbnonu, nbnore, nddl1, nddlb
    integer :: nel, niv, nlag, nma, nn
    integer :: ns, numa, nunoel
    integer ::  vali(5)
    integer, pointer :: v_nnli(:) => null()
    integer, pointer :: adli(:) => null()
    integer, pointer :: bid(:) => null()
    integer, pointer :: adne(:) => null()
    integer, pointer :: qrns(:) => null()
    character(len=24), pointer :: slvk(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
!     NBNOM  : NOMBRE DE NOEUDS DU MAILLAGE
!     DERLI  : NOM DE L'OBJET NU.DERLI CREE SUR 'V'
!              DERLI(N3)= MAX DES N1 TELS QUE IL EXISTE UNE MAILLE SUP
!              DE TYPE SEG3 MS TELLE QUE N1 1ER, N3 3EME NOEUD DE MS
!     DSCLAG : NOM DE L'OBJET NU.DSCLAG CREE SUR 'V'
!              DIM=3*NBRE LE LAGR.
!              SI ILAG LAGRANGE DE BLOCAGE
!              DSCLAG(3*(ILAG-1)+1)= +NUM DU NOEUD PH. BLOQUE
!              DSCLAG(3*(ILAG-1)+2)= -NUM DU DDL DU NOEUD PH. BLOQUE
!              DSCLAG(3*(ILAG-1)+3)= +1 SI 1ER LAGR.
!                                    +2 SI 2EME LAGR.
!              SI ILAG LAGRANGE DE LIAISON
!              DSCLAG(3*(ILAG-1)+1)= 0
!              DSCLAG(3*(ILAG-1)+2)= 0
!              DSCLAG(3*(ILAG-1)+3)= +1 SI 1ER LAGR.
!                                    +2 SI 2EME LAGR.
!-----------------------------------------------------------------------
!     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
!     S.D. MANIPULEES DANS LE SOUS PROGRAMME
!-----------------------------------------------------------------------

!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS LIEL DES S.D. LIGREL
!     REPERTORIEES DANS LE CHAMP LILI DE NUME_DDL
!     ZZLIEL(ILI,IGREL,J) =
!      SI LA JIEME MAILLE DU LIEL IGREL DU LIGREL ILI EST:
!          -UNE MAILLE DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
!          -UNE MAILLE TARDIVE : -POINTEUR DANS LE CHAMP .NEMA

#define zzliel(ili,igrel,j) zi(adli(1+3*(ili-1)+1)-1+ zi(adli(1+3*(ili-1)+2)+igrel-1)+j-1)

!---- NBRE DE GROUPES D'ELEMENTS (DE LIEL) DU LIGREL ILI

#define zzngel(ili) adli(1+3* (ili-1))

!---- NBRE DE NOEUDS DE LA MAILLE TARDIVE IEL ( .NEMA(IEL))
!     DU LIGREL ILI REPERTOIRE .LILI
!     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).NEMA(IEL) )

#define zznsup(ili,iel) zi(adne(1+3* (ili-1)+2)+iel) - zi(adne(1+3*(ili-1)+2)+iel-1 ) - 1

!---- NBRE D ELEMENTS DU LIEL IGREL DU LIGREL ILI DU REPERTOIRE .LILI
!     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).LIEL(IGREL) )

#define zznelg(ili,igrel) zi(adli(1+3*(ili-1)+2)+igrel) - zi(adli(1+3*(ili-1)+2)+igrel-1) - 1

!---- NBRE D ELEMENTS SUPPLEMENTAIRE (.NEMA) DU LIGREL ILI DE .LILI

#define zznels(ili) adne(1+3* (ili-1))

!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS NEMA DES S.D. LIGREL
!     REPERTORIEES DANS LE CHAMP LILI DE NUME_DDL
!     ZZNEMA(ILI,IEL,J) =  1.LE. J .GE. ZZNELS(ILI)
!      SI LE J IEME NOEUD DE LA MAILE TARDIVE IEL DU LIGREL ILI EST:
!          -UN NOEUD DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
!          -UN NOEUD TARDIF : -SON NUMERO DANS LA NUMEROTATION LOCALE
!                              AU LIGREL ILI
!     ZZNEMA(ILI,IEL,ZZNELS(ILI)+1)=NUMERO DU TYPE_MAILLE DE LA MAILLE
!                                   IEL DU LIGREL ILI

#define zznema(ili,iel,j) zi( adne(1+3* (ili-1)+1)-1+ zi(adne(1+3* (ili-1)+2)+iel-1 )+j-1 )

!---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS PRNO DES S.D. LIGREL
!     REPERTORIEES DANS LE CHAMP LILI DE NUME_DDL ET A LEURS ADRESSES
!     ZZPRNO(ILI,NUNOEL,1) = NUMERO DE L'EQUATION ASSOCIEES AU 1ER DDL
!                            DU NOEUD NUNOEL DANS LA NUMEROTATION LOCALE
!                            AU LIGREL ILI DE .LILI
!     ZZPRNO(ILI,NUNOEL,2) = NOMBRE DE DDL PORTES PAR LE NOEUD NUNOEL
!     ZZPRNO(ILI,NUNOEL,2+1) = 1ER CODE
!     ZZPRNO(ILI,NUNOEL,2+NEC) = NEC IEME CODE

#define izzprn(ili,nunoel,l) (idprn1-1+zi(idprn2+ili-1)+ (nunoel-1)* (nec+2)+l-1)
#define zzprno(ili,nunoel,l) zi( idprn1-1+zi(idprn2+ili-1)+ (nunoel-1)* (nec+2)+l-1)

!---- FONCTION D ACCES AUX ELEMENTS DES OBJETS VSUIV ET PSUIV DE LA
!     BASE VOLATILE
!     LES NOEUDS SUP SONT DES LAGRANGE. ON GENERE 2 LAGRANGE
!     "LAGR1" ET "LAGR2" PAR CONDITION DE BLOCAGE ET AUTANT DE MAILLE
!     SUP MSI QU'IL Y A DE NOEUDS NI CONCERNES PAR LE BLOCAGE,ON NOTE
!     NIINF LE NOEUD DE NI NUMERO INF ET NIMAX CELUI DE NUMERO MAX
!     CES MAILLES SUP MSI SONT CARACTERISEES PAR :
!     - TYPE_ELEM = SEG3 -1ER NOEUD NI -2EME NOEUD "LAGR1"-3EME "LAGR2"
!     PAR CONVENTION LE "LAGR1" EST NUMEROTE A LA SUITE DE NIINF-1
!     ET LE "LAGR2" EST NUMEROTE A LA SUITE DE NIMAX
!     SUIVDI(I) = PSUIV(I+1)-PSUIV(I) : NBRE DE NOEUDS SUP "LAGR1" EN
!     RELATION AVEC NI+1 + NBRE DE "LAGR2" EN RELATION AVEC NI
!     SUIVDI(I,J)= NUMERO DU JEME NOEUD SUP SI IL EST A NUMEROTE APRES
!     LE NOEUD NI, -1 "SINON" (NI/= NIMAX DU BLOCAGE RELATIF A J)
#define suivdi(i) zi(ipsuiv+i) - zi(ipsuiv+i-1)
#define idsuiv(i,j) ivsuiv + (zi(ipsuiv+i-1)+j-1) - 1
#define suiv(i,j) zi(ivsuiv+ (zi(ipsuiv+i-1)+j-1)-1)
!
! --------------------------------------------------------------------------------------------------
!
!    call jemarq() FORBIDDEN !
!
    call infniv(ifm, niv)
    nume_ddl = nume_ddlz
!
! - Local mode
!
    moloc = ' '
    if (present(modelocz)) then
        moloc = modelocz
    endif

! --- SI LE CONCEPT : NU EXISTE DEJA, ON LE DETRUIT COMPLETEMENT :
!     ----------------------------------------------------------
    call detrsd('NUME_DDL', nume_ddl)

! --- NOMS DES PRINCIPAUX OBJETS JEVEUX :
!     ---------------------------------
    prof_chno = nume_ddl//'.NUME'
    lili      = prof_chno(1:19)//'.LILI'
    prno      = prof_chno(1:19)//'.PRNO'
    nueq      = prof_chno(1:19)//'.NUEQ'
    nume_equa = nume_ddl//'.NUME'
    nequ      = nume_equa(1:19)//'.NEQU'
    refn      = nume_equa(1:19)//'.REFN'
    nnli      = nume_ddl//'.NNLI'
    nuno      = nume_ddl//'.NUNO'
    exi1      = nume_ddl//'.EXI1'
    newn      = nume_ddl//'.NEWN'
    oldn      = nume_ddl//'.OLDN'
    derli     = nume_ddl//'.DERLI'
    psuiv     = nume_ddl//'.PSUIVE'
    lsuiv     = nume_ddl//'.LSUIVE'
    vsuiv     = nume_ddl//'.VSUIVE'
    num21     = nume_ddl//'.NUM21'
    num2      = nume_ddl//'.NUM2'
    dsclag    = nume_ddl//'.DESCLAG'
!
! - Create LILI objects
!
    call nulili(nb_ligr, list_ligr, lili, base(2:2), gran_name,&
                igds   , mesh     , nec , nlili    , modelocz = moloc)
    call jeveuo(nume_ddl//'     .ADLI', 'E', vi=adli)
    call jeveuo(nume_ddl//'     .ADNE', 'E', vi=adne)
!
! - Access to mesh objects
!
    call jeexin(mesh(1:8)//'.CONNEX', iret)
    if (iret .gt. 0) then
        call jeveuo(mesh(1:8)//'.CONNEX', 'L', iconx1)
        call jeveuo(jexatr(mesh(1:8)//'.CONNEX', 'LONCUM'), 'L', iconx2)
    endif
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_node_mesh)
    call dismoi('NB_NL_MAILLA', mesh, 'MAILLAGE', repi=nb_lagr_mesh)
    nb_node = nb_node_mesh + nb_lagr_mesh

! --- LILI(1)='&MAILLA'
!     -----------------
    ilim = 1

! --- ALLOCATION DE L'OBJET NU.NNLI NOMBRE DE NOEUDS DECLARES DANS
! --- LE LIGREL ILI DE LILI :
!     ---------------------
    call jecreo(nnli, 'V V I')
    call jeecra(nnli, 'LONMAX', nlili)
    call jeveuo(nnli, 'E', vi = v_nnli)
    call jecrec(nuno, 'V V I ', 'NU', 'CONTIG', 'VARIABLE',&
                nlili)
    v_nnli(1) = nb_node

! --- ALLOCATION DE PRNO :
!     -------------------------------------------------
    call jecrec(prno, base(2:2)//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                nlili)


! --- CALCUL DE N, CALCUL DES NNLI ET DU POINTEUR DE LONGUEUR DE
! --- PRNO :
! --- NBNOM NOMBRE DE NOEUDS TOTAL DU MAILLAGE :
!     ------------------------------------------

    call jeecra(jexnum(nuno, 1), 'LONMAX', nb_node)
    call jeecra(jexnum(prno, 1), 'LONMAX', nb_node* (nec+2))


! --- N CONTIENDRA LE NOMBRE TOTAL (MAX) DE NOEUDS DE NUME_DDL
! --- TOUS LES NOEUDS DU MAILLAGE + TOUS LES NOEUDS SUPL. DES LIGRELS :
!     ---------------------------------------------------------------
    n = nb_node
    do ili = 2, nlili
        call jenuno(jexnum(lili, ili), nomli)
        call jeexin(nomli(1:19)//'.NBNO', iret)
        if (iret .ne. 0) then

! ---    ACCES AU NOMBRE DE NOEUDS SUP DU LIGREL DE NOM NOMLI :
!        ----------------------------------------------------
            call jeveuo(nomli(1:19)//'.NBNO', 'L', iad)
            nbn = zi(iad)
        else
            nbn = 0
        endif

! ---    AFFECTATION DU CHAMP .NNLI DE NU :
!        --------------------------------
        v_nnli(ili) = nbn
        call jeecra(jexnum(nuno, ili), 'LONMAX', nbn)

        call jeecra(jexnum(prno, ili), 'LONMAX', nbn* (nec+2))
        n = n + nbn
    end do

    call jeveuo(prno, 'E', idprn1)
    call jeveuo(jexatr(prno, 'LONCUM'), 'L', idprn2)


! --- ALLOCATION DE LA COLLECTION NUNO NUMEROTEE DE VECTEUR DE LONGUEUR
! --- NNLI ET DE NBRE D'ELMT NLILI SUR LA BASE VOLATILE (.NUNO ET .NNLI
! --- SONT SUPPRIMES DE LA S.D. NUME_EQUA) :
!     ------------------------------------
    call jeveuo(nuno, 'E', inuno1)
    call jeveuo(jexatr(nuno, 'LONCUM'), 'L', inuno2)
    nlag = zi(inuno2+nlili) - zi(inuno2+1)


! --- RENUMEROTATION , CREATION DES OBJETS NU.EXI1, NU.NEWN ET NU.OLDN :
!     ----------------------------------------------------------------

    call renuno(nume_ddl, renumz)
    call jeveuo(exi1, 'L', iexi1)
    call jeveuo(newn, 'L', inewn)
    call jeveuo(oldn, 'L', ioldn)

! --- ALLOCATION DE PSUIV LSUIV OBJETS DE LA BASE VOLATILE :
!     ----------------------------------------------------
    call wkvect(psuiv, ' V V I', n+2, ipsuiv)
    call wkvect(lsuiv, ' V V I', n+1, ilsuiv)

! --- CALCUL DE  PSUIV
! --- 1ERE ETAPE :
! --- PSUIV(K)= NOMBRE DE NOEUDS SUP A ECRIRE DERRIERE K :
!     ==================================================
    do ili = 2, nlili

! ---  INULAG EST UN INDICATEUR PERMETTANT DE SAVOIR SI LE
! ---  LIGREL CONTIENT DES MAILLES DE LAGRANGE (INFORMATION
! ---  EQUIVALENTE A L'EXISTENCE DE L'OBJET LIGREL.LGNS) :
!      -------------------------------------------------
        inulag = 0
        call jenuno(jexnum(lili, ili), nomli)
        call jeexin(nomli(1:19)//'.LGNS', iret)
        if (iret .ne. 0) then

! ---  SIGNIFICATION DE LIRGEL.LGNS :
! ---  C'EST L'INDICATEUR DE POSITION DU PREMIER LAGRANGE
! ---  SI = +1 ON LE PLACE AVANT LE PREMIER DDL PHYSIQUE
! ---          CONCERNE PAR LA RELATION OU LE BLOCAGE
! ---  SI = -1 ON LE PLACE APRES LE DERNIER DDL PHYSIQUE
! ---          CONCERNE PAR LA RELATION OU LE BLOCAGE :
!              --------------------------------------
            call jeveuo(nomli(1:19)//'.LGNS', 'L', idlgns)
            inulag = 1
        endif

        do iel = 1, zznels(ili)
            nn = zznsup(ili,iel)

! ---   DOUBLE LAGRANGE :
! ---   N2 DERRIERE LE NOEUD DU MAILLAGE DE NUMERO NEWN(N1)-1
! ---   SI L'INDICATEUR DE POSITION DU PREMIER LAGRANGE = +1
! ---   N2 DERRIERE LE NOEUD DU MAILLAGE DE NUMERO NEWN(N1)
! ---   SI L'INDICATEUR DE POSITION DU PREMIER LAGRANGE = -1
! ---   N3 DERRIERE LE NOEUD DU MAILLAGE NEW(N1) :
! ---   SI L'INDICATEUR DE POSITION DU PREMIER LAGRANGE = +1
! ---   N3 DERRIERE N2
! ---   SI L'INDICATEUR DE POSITION DU PREMIER LAGRANGE = -1 :
!       ----------------------------------------------------
            if (nn .eq. 3) then
                n1 = zznema(ili,iel,1)
                n2 = zznema(ili,iel,2)
                n3 = zznema(ili,iel,3)
                if (((n1.gt.0).and. (n2.lt.0)) .and. (n3.lt.0)) then
                    n21 = -n2
                    n1re = zi(inewn-1+n1)
                    n1m1re = n1re - 1

! ---  RECUPERATION DE L'INDICATEUR DE POSITION DU PREMIER LAGRANGE :
!      ------------------------------------------------------------
                    if (inulag .eq. 1) then

! ---    LE PREMIER LAGRANGE EST PLACE AVANT LE PREMIER DDL PHYSIQUE
! ---    CONCERNE PAR LE BLOCAGE OU LA RELATION :
!        --------------------------------------
                        if (zi(idlgns+n21-1) .eq. 1) then
                            n1m1re = n1re - 1

! ---    LE PREMIER LAGRANGE EST PLACE ALORS  APRES LE DERNIER DDL
! ---    PHYSIQUE CONCERNE PAR LE BLOCAGE OU LA RELATION :
!        ----------------------------------------------
                        else if (zi(idlgns+n21-1).eq.-1) then
                            n1m1re = n1re
                        else
                            call utmess('F', 'ASSEMBLA_27')
                        endif
                    endif

                    zi(ipsuiv+n1m1re) = zi(ipsuiv+n1m1re) + 1
                    zi(ipsuiv+n1re) = zi(ipsuiv+n1re) + 1
                endif
            endif
        end do
    end do

! --- 2EME ETAPE :
! --- PSUIV = PROFIL DE PSUIV :
!     =======================
    l1 = zi(ipsuiv)
    zi(ipsuiv) = 1
    do i = 1, n + 1
        l2 = zi(ipsuiv+i)
        zi(ipsuiv+i) = zi(ipsuiv+i-1) + l1
        l1 = l2
    end do

! --- ALLOCATION DE VSUIV :
!     -------------------
    long = zi(ipsuiv+n+1) - 1
    if (long .gt. 0) then
        call wkvect(vsuiv, ' V V I', long, ivsuiv)
        call wkvect(derli, ' V V I', n+1, iderli)

! --- ALLOCATION DE DSCLAG LE DESCRIPTEUR DES "LAGRANGE"
! --- DIM(DSCLAG) = 3*NLAG NLAG:NOMBRE TOTAL DE "LAGRANGE"
! --- LES "LAGRANGE" SONT NUMEROTES SUR LA NUMEROTATION GLOBALE DE
! --- 1ER NIVEAU EN OUBLIANT LES NOEUDS DU MAILLAGE :
!     ---------------------------------------------
        call wkvect(dsclag, ' V V I', 3*nlag, iddlag)
    endif

! --- CALCUL DE VSUIV ET LSUIV :
!     ========================
    do ili = 2, nlili

! ---  INULAG EST UN INDICATEUR PERMETTANT DE SAVOIR SI LE
! ---  LIGREL CONTIENT DES MAILLES DE LAGRANGE (INFORMATION
! ---  EQUIVALENTE A L'EXISTENCE DE L'OBJET LIGREL.LGNS) :
!      -------------------------------------------------
        inulag = 0
        call jenuno(jexnum(lili, ili), nomli)
        call jeexin(nomli(1:19)//'.LGNS', iret)
        if (iret .ne. 0) then

! ---  SIGNIFICATION DE LIRGEL.LGNS :
! ---  C'EST L'INDICATEUR DE POSITION DU PREMIER LAGRANGE
! ---  SI = +1 ON LE PLACE AVANT LE PREMIER DDL PHYSIQUE
! ---          CONCERNE PAR LA RELATION OU LE BLOCAGE
! ---  SI = -1 ON LE PLACE APRES LE DERNIER DDL PHYSIQUE
! ---          CONCERNE PAR LA RELATION OU LE BLOCAGE :
!              --------------------------------------
            call jeveuo(nomli(1:19)//'.LGNS', 'L', idlgns)
            inulag = 1
        endif

        do iel = 1, zznels(ili)
            nn = zznsup(ili,iel)
            if (nn .eq. 3) then
                n1 = zznema(ili,iel,1)
                n2 = zznema(ili,iel,2)
                n3 = zznema(ili,iel,3)
                if (((n1.gt.0).and. (n2.lt.0)) .and. (n3.lt.0)) then

! ---    TRANSFORMATION DE N2 , NUMERO DU PREMIER LAGRANGE DANS LA
! ---    NUMEROTATION LOCALE AU LIGREL EN SON NUMERO DANS LA
! ---    NUMEROTATION GLOBALE :
!        --------------------
                    n21 = -n2
                    n2 = -n2
                    n2 = zi(inuno2+ili-1) + n2 - 1
                    ilag2 = n2 - nb_node
                    n1re = zi(inewn-1+n1)
                    n1m1re = n1re - 1

! ---  JNULAG EST UN INDICATEUR PERMETTANT DE SAVOIR OU L'ON
! ---  DOIT PLACER LE PREMIER LAGRANGE :
! ---  SI = 0 C'EST LA NUMEROTATION TRADITIONNELLE, ON PLACE LE
! ---         LE PREMIER LAGRANGE AVANT LE PREMIER DDL PHYSIQUE
! ---         CONCERNE PAR LA RELATION OU LE BLOCAGE
! ---  SI = 1 ON PLACE LE LE PREMIER LAGRANGE APRES LE DERNIER DDL
! ---          PHYSIQUE CONCERNE PAR LA RELATION OU LE BLOCAGE :
!              -----------------------------------------------
                    jnulag = 0


! ---  RECUPERATION DE L'INDICATEUR DE POSITION DU PREMIER LAGRANGE :
!      ------------------------------------------------------------
                    if (inulag .eq. 1) then

! ---    LE PREMIER LAGRANGE EST PLACE AVANT LE PREMIER DDL PHYSIQUE
! ---    CONCERNE PAR LE BLOCAGE OU LA RELATION :
!        --------------------------------------
                        if (zi(idlgns+n21-1) .eq. 1) then
                            jnulag = 0

! ---    LE PREMIER LAGRANGE EST PLACE APRES LE DERNIER DDL PHYSIQUE
! ---    CONCERNE PAR LE BLOCAGE OU LA RELATION :
!        --------------------------------------
                        else if (zi(idlgns+n21-1).eq.-1) then
                            jnulag = 1
                        else
                            call utmess('F', 'ASSEMBLA_27')
                        endif
                    endif

! ---    CAS JNULAG = 0 : ON PLACE LE PREMIER LAGRANGE AVANT LE
! ---    PREMIER DDL PHYSIQUE :
! ---    ZI(ILSUIV+N1M1RE) EST LE COMPTEUR DU NOMBRE DE LAGRANGE
! ---    A PLACER AVANT N1
! ---    ZI(IDSUIV(N1M1RE+1,ZI(ILSUIV+N1M1RE)))  EST LE NUMERO
! ---    DU LAGRANGE (DANS LA NUMEROTATION GLOBALE) PRECEDANT N1 ET
! ---    D'INDICE ZI(ILSUIV+N1M1RE) (DANS LA LISTE DES LAGRANGE
! ---    PRECEDANT N1) :
!        -------------
                    if (jnulag .eq. 0) then
                        zi(ilsuiv+n1m1re) = zi(ilsuiv+n1m1re) + 1
                        zi(idsuiv(n1m1re+1,zi(ilsuiv+n1m1re))) = n2
                    endif

! ---    TRANSFORMATION DE N3 , NUMERO DU SECOND LAGRANGE DANS LA
! ---    NUMEROTATION LOCALE AU LIGREL EN SON NUMERO DANS LA
! ---    NUMEROTATION GLOBALE :
!        --------------------
                    n3 = -n3
                    n3 = zi(inuno2+ili-1) + n3 - 1
                    ilag3 = n3 - nb_node

! ---    RECUPERATION DU NOEUD PHYSIQUE DE NUMERO LE PLUS GRAND
! ---    LIE AU SECOND LAGRANGE PAR LE TABLEAU DERLI, CETTE
! ---    VALEUR N'EST DIFFERENTE DE 0 QUE S'IL S'AGIT D'UNE
! ---    RELATION LINEAIRE :
!        -----------------
                    n0 = zi(iderli+n3)

                    zi(iddlag+3* (ilag2-1)+1) = -1
                    zi(iddlag+3* (ilag3-1)+1) = -1
                    zi(iddlag+3* (ilag2-1)+2) = 1
                    zi(iddlag+3* (ilag3-1)+2) = 2

! ---    CAS DES RELATIONS LINEAIRES ENTRE DDLS
!        ======================================
! ---    TRAITEMENT DU PREMIER LAGRANGE
!        -----------------------------
! ---    DANS LE CAS JNULAG = 0 :
! ---    LE TRAITEMENT DU PREMIER LAGRANGE A DEJA ETE FAIT :
! ---    ON LE MET SYSTEMATIQUEMENT AVANT LE NOEUD PHYSIQUE
! ---    DE L'ELEMENT DE LAGRANGE COURANT
! ---    C'EST AU MOMENT DE LA RENUMEROTATION OU L'ON COMMENCE
! ---    PAR LES NOEUDS DE PLUS PETIT INDICE QUE L'ON DECIDERA,
! ---    SI UN NUMERO LUI A DEJA ETE ATTRIBUE , DE NE PLUS LE
! ---    NUMEROTER.

! ---    DANS LE CAS JNULAG = 1 :
! ---    ON TRAITE LE PREMIER  LAGRANGE COMME LE SECOND :
! ---    ON PLACE LE PREMIER LAGRANGE APRES LE NOEUD PHYSIQUE
! ---    DE L'ELEMENT DE LAGRANGE COURANT
! ---    SI LE NUMERO DU NOEUD PHYSIQUE DE L'ELEMENT DE LAGRANGE
! ---    EST PLUS PETIT QUE LE PLUS GRAND NUMERO DE NOEUD PHYSIQUE
! ---    LIE AU SECOND LAGRANGE (PAR LE TABLEAU DERLI)
! ---    ON DESACTIVE LE POSITIONNEMENT DU LAGRANGE APRES CE NUMERO
! ---    EN LUI AFFECTANT UNE VALEUR -1 DANS LE TABLEAU SUIV

! ---    TRAITEMENT DU SECOND LAGRANGE
!        -----------------------------
! ---    DANS LES CAS JNULAG = 0 ET JNULAG = 1, ON TRAITE
! ---    LE SECOND LAGRANGE COMME CE QUI EST ENONCE CI-DESSUS
! ---    POUR LE PREMIER LAGRANGE DANS LE CAS JNULAG = 1,
! ---    A CECI PRES QUE DANS CE DERNIER CAS, ON PLACE
! ---    LE SECOND LAGRANGE APRES LE PREMIER :
!        -----------------------------------
                    if (n0 .gt. 0) then
                        zi(iddlag+3* (ilag2-1)) = 0
                        zi(iddlag+3* (ilag3-1)) = 0
                        zi(iddlag+3* (ilag2-1)+1) = 0
                        zi(iddlag+3* (ilag3-1)+1) = 0
                        n0re = zi(inewn-1+n0)

! ---    TRAITEMENT D 'UN ELEMENT DE LAGRANGE DONT LE NUMERO DU
! ---    NOEUD PHYSIQUE EST PLUS GRAND QUE LE PLUS GRAND NUMERO
! ---    DE NOEUD PHYSIQUE LIE PAR LA MEME RELATION LINEAIRE
! ---    ET DEJA TRAITE :
!        --------------
                        if (n0re .lt. n1re) then
                            icer1 = 0
                            icer2 = 0
                            if (jnulag .eq. 1) then
                                do j = 1, suivdi(n0re+1)
                                    ns = suiv(n0re+1,j)
                                    if (ns .eq. n2) then
                                        if (icer1 .ne. 0) then
                                            vali (1) = n2
                                            vali (2) = n1
                                            call utmess('F', 'ASSEMBLA_63', ni=2, vali=vali)
                                        endif
                                        icer1 = icer1 + 1
                                        zi(idsuiv(n0re+1,j)) = -1
                                    endif
                                end do
                            endif
                            do j = 1, suivdi(n0re+1)
                                ns = suiv(n0re+1,j)
                                if (ns .eq. n3) then
                                    if (icer2 .ne. 0) then
                                        vali (1) = n3
                                        vali (2) = n1
                                        call utmess('F', 'ASSEMBLA_64', ni=2, vali=vali)
                                    endif
                                    icer2 = icer2 + 1
                                    zi(idsuiv(n0re+1,j)) = -1
                                endif
                            end do

! ---    CAS JNULAG = 1 : ON PLACE LE PREMIER LAGRANGE APRES LE
! ---    NOEUD PHYSIQUE COURANT:
! ---    ZI(ILSUIV+N1RE) EST LE COMPTEUR DU NOMBRE DE LAGRANGE
! ---    A PLACER APRES N1
! ---    ZI(IDSUIV(N1RE+1,ZI(ILSUIV+N1RE)))  EST LE NUMERO
! ---    DU LAGRANGE (DANS LA NUMEROTATION GLOBALE) SUIVANT N1 ET
! ---    D'INDICE ZI(ILSUIV+N1RE) (DANS LA LISTE DES LAGRANGE
! ---    SUIVANT N1) :
!        -----------
                            if (jnulag .eq. 1) then
                                zi(ilsuiv+n1re) = zi(ilsuiv+n1re) + 1
                                zi(idsuiv(n1re+1,zi(ilsuiv+n1re))) =&
                                n2
                            endif

! ---    ON PLACE LE SECOND LAGRANGE APRES LE NOEUD PHYSIQUE
! ---    COURANT:
! ---    ZI(ILSUIV+N1RE) EST LE COMPTEUR DU NOMBRE DE LAGRANGE
! ---    A PLACER APRES N1
! ---    ZI(IDSUIV(N1RE+1,ZI(ILSUIV+N1RE)))  EST LE NUMERO
! ---    DU LAGRANGE (DANS LA NUMEROTATION GLOBALE) SUIVANT N1 ET
! ---    D'INDICE ZI(ILSUIV+N1RE) (DANS LA LISTE DES LAGRANGE
! ---    SUIVANT N1) :
!        -----------
                            zi(ilsuiv+n1re) = zi(ilsuiv+n1re) + 1
                            zi(idsuiv(n1re+1,zi(ilsuiv+n1re))) = n3
                            zi(iderli+n3) = n1
                        else

! ---    TRAITEMENT D 'UN ELEMENT DE LAGRANGE DONT LE NUMERO DU
! ---    NOEUD PHYSIQUE EST PLUS PETIT QUE LE PLUS GRAND NUMERO
! ---    DE NOEUD PHYSIQUE LIE PAR LA MEME RELATION LINEAIRE
! ---    ET DEJA TRAITE :
!        --------------
                            if (jnulag .eq. 1) then

! ---    CAS JNULAG = 1 : ON PLACE LE PREMIER LAGRANGE APRES LE
! ---    NOEUD PHYSIQUE COURANT:
! ---    ZI(ILSUIV+N1RE) EST LE COMPTEUR DU NOMBRE DE LAGRANGE
! ---    A PLACER APRES N1
! ---    ZI(IDSUIV(N1RE+1,ZI(ILSUIV+N1RE)))  EST LE NUMERO
! ---    DU LAGRANGE (DANS LA NUMEROTATION GLOBALE) SUIVANT N1 ET
! ---    D'INDICE ZI(ILSUIV+N1RE) (DANS LA LISTE DES LAGRANGE
! ---    SUIVANT N1), ON DESACTIVE CE POSITIONNEMENT EN METTANT CE
! ---    NUMERO A -1 :
!        -----------
                                zi(ilsuiv+n1re) = zi(ilsuiv+n1re) + 1
                                zi(idsuiv(n1re+1,zi(ilsuiv+n1re))) = - 1
                            endif

! ---    ON PLACE LE SECOND LAGRANGE APRES LE NOEUD PHYSIQUE
! ---    COURANT:
! ---    ZI(ILSUIV+N1RE) EST LE COMPTEUR DU NOMBRE DE LAGRANGE
! ---    A PLACER APRES N1
! ---    ZI(IDSUIV(N1RE+1,ZI(ILSUIV+N1RE)))  EST LE NUMERO
! ---    DU LAGRANGE (DANS LA NUMEROTATION GLOBALE) SUIVANT N1 ET
! ---    D'INDICE ZI(ILSUIV+N1RE) (DANS LA LISTE DES LAGRANGE
! ---    SUIVANT N1), ON DESACTIVE CE POSITIONNEMENT EN METTANT CE
! ---    NUMERO A -1 :
!        -----------
                            zi(ilsuiv+n1re) = zi(ilsuiv+n1re) + 1
                            zi(idsuiv(n1re+1,zi(ilsuiv+n1re))) = -1
                        endif
                    else

! ---    CAS DES BLOCAGES
!        ================
! ---    TRAITEMENT DU PREMIER LAGRANGE
!        -----------------------------
! ---    DANS LE CAS JNULAG = 0 :
! ---    LE TRAITEMENT DU PREMIER LAGRANGE A DEJA ETE FAIT :
! ---    ON LE MET SYSTEMATIQUEMENT AVANT LE NOEUD PHYSIQUE
! ---    DE L'ELEMENT DE LAGRANGE COURANT

! ---    DANS LE CAS JNULAG = 1 :
! ---    ON TRAITE LE PREMIER  LAGRANGE COMME LE SECOND :
! ---    ON PLACE LE PREMIER LAGRANGE APRES LE NOEUD PHYSIQUE
! ---    BLOQUE

! ---    TRAITEMENT DU SECOND LAGRANGE
!        -----------------------------
! ---    DANS LES CAS JNULAG = 0 ET JNULAG = 1, ON TRAITE
! ---    LE SECOND LAGRANGE COMME CE QUI EST ENONCE CI-DESSUS
! ---    POUR LE PREMIER LAGRANGE DANS LE CAS JNULAG = 1,
! ---    A CECI PRES QUE DANS CE DERNIER CAS, ON PLACE
! ---    LE SECOND LAGRANGE APRES LE PREMIER :
!        -----------------------------------
                        zi(iddlag+3* (ilag2-1)) = n1
                        zi(iddlag+3* (ilag3-1)) = n1

! ---    CAS JNULAG = 1 : ON PLACE LE PREMIER LAGRANGE APRES LE
! ---    NOEUD PHYSIQUE :
! ---    ZI(ILSUIV+N1RE) EST LE COMPTEUR DU NOMBRE DE LAGRANGE
! ---    A PLACER APRES N1
! ---    ZI(IDSUIV(N1RE+1,ZI(ILSUIV+N1RE)))  EST LE NUMERO
! ---    DU LAGRANGE (DANS LA NUMEROTATION GLOBALE) SUIVANT N1 ET
! ---    D'INDICE ZI(ILSUIV+N1RE) (DANS LA LISTE DES LAGRANGE
! ---    SUIVANT N1) :
!        -----------
                        if (jnulag .eq. 1) then
                            zi(ilsuiv+n1re) = zi(ilsuiv+n1re) + 1
                            zi(idsuiv(n1re+1,zi(ilsuiv+n1re))) = n2
                        endif

! ---    ON PLACE LE SECOND LAGRANGE APRES LE NOEUD PHYSIQUE :
! ---    ZI(ILSUIV+N1RE) EST LE COMPTEUR DU NOMBRE DE LAGRANGE
! ---    A PLACER APRES N1
! ---    ZI(IDSUIV(N1RE+1,ZI(ILSUIV+N1RE)))  EST LE NUMERO
! ---    DU LAGRANGE (DANS LA NUMEROTATION GLOBALE) SUIVANT N1 ET
! ---    D'INDICE ZI(ILSUIV+N1RE) (DANS LA LISTE DES LAGRANGE
! ---    SUIVANT N1) :
!        -----------
                        zi(ilsuiv+n1re) = zi(ilsuiv+n1re) + 1
                        zi(idsuiv(n1re+1,zi(ilsuiv+n1re))) = n3
                        zi(iderli+n3) = n1
                    endif
                endif
            endif
        end do
    end do


! --- RENUMEROTATION :
!     ==============

! ---  ALLOCATION DE NUM21 ET NUM2
! ---  NUM2(I) = NUMERO DANS LA NOUVELLE NUMEROTATION NUNO DU NOEUD I
! ---  DANS LA NUMEROTATION GLOBALE DE 1ER NIVEAU
! ---  SI NUM2(I)=J ALORS NUM21(J)=I :
!      -----------------------------
    call wkvect(num21, ' V V I', n+1, inum21)
    call wkvect(num2, ' V V I', n+1, inum2)

! ---  CALCUL DE NUM2 ET NUM21 QUI REPRESENTE "L'INVERSE" DE NUM2
! ---  NBNONU : NOMBRE DE NOEUDS NUMEROTES DANS NUNO :
!      ---------------------------------------------
    nbnonu = 0

! ---  BOUCLE SUR LES LAGRANGE PRECEDANT LE PREMIER NOEUD PHYSIQUE :
!      -----------------------------------------------------------
    do j = 1, suivdi(1)
        j1 = suiv(1,j)

! ---  SI LE LAGRANGE N'A PAS ETE RENUMEROTE, ON LE RENUMEROTE :
!      -------------------------------------------------------
        if (zi(inum2+j1) .eq. 0) then
            nbnonu = nbnonu + 1
            zi(inum2+j1) = nbnonu
            zi(inum21+nbnonu) = j1
        endif
    end do

! ---  NBNORE EST LE NOMBRE DE NOEUDS DU MAILLAGE PARTICIPANTS A LA
! ---  NUMEROTATION:
!      ------------
    call jelira(oldn, 'LONUTI', nbnore)

! ---  BOUCLE SUR LES NOEUDS PHYSIQUES :
!      -------------------------------
    do ire = 1, nbnore
        i = zi(ioldn-1+ire)

! ---  SI LE NOEUD PHYSIQUE N'A PAS ETE RENUMEROTE, ON LE RENUMEROTE :
!      -------------------------------------------------------------
        if (zi(inum2+i) .eq. 0) then
            nbnonu = nbnonu + 1
            zi(inum2+i) = nbnonu
            zi(inum21+nbnonu) = i

! ---  BOUCLE SUR LES LAGRANGE SUIVANT LE NOEUD PHYSIQUE COURANT :
!      ---------------------------------------------------------
            do j = 1, suivdi(ire+1)
                j1 = suiv(ire+1,j)

! ---  ON NE PREND EN COMPTE QUE LES LAGRANGE AYANT UN INDICE 'ACTIF':
!      -------------------------------------------------------------
                if (j1 .gt. 0) then

! ---    SI LE LAGRANGE N'A PAS ETE RENUMEROTE, ON LE RENUMEROTE :
!        -------------------------------------------------------
                    if (zi(inum2+j1) .eq. 0) then
                        nbnonu = nbnonu + 1
                        zi(inum2+j1) = nbnonu
                        zi(inum21+nbnonu) = j1
                    endif
                endif
            end do
        endif
    end do

    if (nbnonu .ne. (nbnore+nlag)) then
        call utmess('F', 'ASSEMBLA_28')
    endif



! --- CALCUL DES DESCRIPTEURS DES PRNO
!     ================================


! --- DETERMINATION DES .PRNM ET DES .PRNS POUR CHAQUE LIGREL :
!     -------------------------------------------------------
    do ili = 2, nlili
        call jenuno(jexnum(lili, ili), nomli)
        call creprn(nomli, moloc, 'V', nomli(1:19)//'.QRNM', nomli(1:19) //'.QRNS')
    end do


! --- 1ERE ETAPE : NOEUDS DU MAILLAGE (PHYSIQUES ET LAGRANGES)
! --- SI NUNOEL NOEUD DU MAILLAGE
! --- PRNO(1,NUNOEL,L+2)= "SIGMA"(.QRNM(MA(NUNOEL))(L))
!     -------------------------------------------------
    do ili = 2, nlili
        call jenuno(jexnum(lili, ili), nomli)
        call jeexin(nomli(1:19)//'.QRNM', iret)
        if (iret .eq. 0) goto 150
        call jeveuo(nomli(1:19)//'.QRNM', 'L', iprnm)

        do i = 1, nbnore
            nunoel = zi(ioldn-1+i)
            do l = 1, nec
                iec = zi(iprnm-1+nec* (nunoel-1)+l)
                zi(izzprn(1,nunoel,l+2)) = ior(zzprno(1,nunoel,l+2), iec)
            end do
        end do
150     continue
    end do


! --- 2EME ETAPE : NOEUDS SUPPLEMENTAIRES DES LIGRELS:
! --- SI NUNOEL NOEUD TARDIF DU LIGREL ILI NOMLI = LILI(ILI)
! --- PRNO(ILI,NUNOEL,L+2)= NOMLI.QRNS(NUNOEL)(L) :
!     -------------------------------------------
    do ili = 2, nlili
        call jenuno(jexnum(lili, ili), nomli)
        call jeveuo(nomli(1:19)//'.QRNM', 'L', iprnm)
        call jeveuo(nomli(1:19)//'.NBNO', 'L', vi=bid)
        if (bid(1) .gt. 0) call jeveuo(nomli(1:19)//'.QRNS', 'L', vi=qrns)

        do igr = 1, zzngel(ili)
            nel = zznelg(ili,igr)

            if (nel .ge. 0) then
                itypel = zzliel(ili,igr,nel+1)
                call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
                icddlb = 0
                nddlb = 0
            endif

            do j = 1, nel
                numa = zzliel(ili,igr,j)
                if (numa .lt. 0) then
                    numa = -numa
                    do k = 1, zznsup(ili, numa)
                        nunoel = zznema(ili,numa,k)

                        if (nunoel .gt. 0) then
                            do l = 1, nec
                                iec = zi(iprnm+nec* (nunoel-1)+l-1)
                                zi(izzprn(ilim,nunoel,l+2)) = ior(&
                                                              zzprno( ilim, nunoel, l+2 ), iec)
                            end do

                        else
                            nunoel = -nunoel

!                 -- CALCUL DU NUMERO DE LA CMP ASSO
                            if (icddlb .eq. 0) then
                                ASSERT(gran_name.eq.nomte(3:8))
                                nomcmp = nomte(10:16)

!                   "GLUTE" POUR TEMP_MIL, TEMP_INF, TEMP_SUP :
                                if (nomcmp .eq. 'TEMP_MI') then
                                    nomcmp = 'TEMP_MIL'
                                else if (nomcmp.eq.'TEMP_IN') then
                                    nomcmp = 'TEMP_INF'
                                else if (nomcmp.eq.'TEMP_SU') then
                                    nomcmp = 'TEMP_SUP'
                                endif

                                call jeveuo(jexnom('&CATA.GD.NOMCMP', gran_name), 'L', idnocm)
                                call jelira(jexnom('&CATA.GD.NOMCMP', gran_name), 'LONMAX', nbcmp,&
                                            kbid)
                                nddlb = indik8(zk8(idnocm),nomcmp,1, nbcmp)
                                ASSERT(nddlb.ne.0)
                                icddlb = 1
                            endif

                            do l = 1, nec
                                iec = qrns(1+nec* (nunoel-1)+l-1)
                                zi(izzprn(ili,nunoel,l+2)) = ior(zzprno( ili, nunoel, l+2), iec)
                            end do

                            ilag = zi(inuno2+ili-1) + nunoel - 1
                            ilag = ilag - nb_node
                            zi(iddlag+3* (ilag-1)+1) = zi(iddlag+3* ( ilag-1)+1 )* nddlb

                        endif
                    end do

                endif

            end do
        end do
    end do


! --- CALCUL DES ADRESSES DANS LES PRNO
!     =================================
    iad = 1

    do i = 1, n
        call nuno1(i, ili, nunoel, n, inum21,&
                   inuno2, nlili)
        if (ili .gt. 0) then
            nddl1 = zzprno(ili,nunoel,2)
            if (nddl1 .eq. 0) then
                nddl1 = nddl(ili,nunoel,nec,idprn1,idprn2)

                zi(izzprn(ili,nunoel,2)) = nddl1
            endif
            zi(izzprn(ili,nunoel,1)) = iad
            iad = iad + nddl1
        endif
    end do

    nequa = iad - 1
    call wkvect(nequ, base(1:1)//' V I', 2, idnequ)
    zi(idnequ) = nequa

    if (niv .ge. 1) then

! ---   CALCUL DE NMA : NOMBRE DE NOEUDS DU MAILLAGE PORTEURS DE DDLS :
!       ----------------------------------------------------------------
        nma = 0
        call jeveuo(jexnum(prno, 1), 'L', jprno)
        do ino = 1, nb_node
            if (zi(jprno-1+ (ino-1)* (2+nec)+2) .gt. 0) nma = nma + 1
        end do
        vali(1) = nequa
        vali(2) = nequa - nlag
        vali(3) = nma
        vali(4) = nlag
        vali(5) = nlag/2
        call utmess('I', 'FACTOR_1', ni=5, vali=vali)
    endif


    call wkvect(refn, base(1:1)//' V K24', 4, idref)
    zk24(idref) = mesh
    zk24(idref+1) = gran_name
    if (present(solver)) then
        call jeveuo(solver(1:19)//'.SLVK', 'L', vk24=slvk)
        zk24(idref+2) = slvk(1)
        zk24(idref+3) = slvk(6)
    endif
!
! - Create NUEQ object
!
    call wkvect(nueq, base(2:2)//' V I', nequa, ianueq)
    do i = 1, nequa
        zi(ianueq-1+i) = i
    end do
!
! - Set DEEQ and DELG objects with non-physical nodes
!
    call nudeeq(mesh , nb_node_mesh, nb_lagr_mesh, base, nume_ddl,&
                nequa, igds        , iddlag)


! --- DESTRUCTION DES .PRNM ET DES .PRNS DE CHAQUE LIGREL :
!     ---------------------------------------------------
    do ili = 1, nlili
        call jenuno(jexnum(lili, ili), nomli)
        call jedetr(nomli(1:19)//'.QRNM')
        call jedetr(nomli(1:19)//'.QRNS')
    end do

    call jedetr(lsuiv)
    call jedetr(psuiv)
    call jedetr(exi1)
    call jedetr(num2)
    call jedetr(num21)
    call jedetr(nuno)
    call jedetr(nnli)
    call jedetr(derli)
    call jedetr(vsuiv)
    call jedetr(dsclag)
!
!    call jedema() FORBIDDEN !
!
end subroutine
