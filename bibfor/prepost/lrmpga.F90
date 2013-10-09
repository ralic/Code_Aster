subroutine lrmpga(nrofic, ligrel, nochmd, nbma, pgmail,&
                  pgmmil, ntypel, npgmax, indpg, numpt,&
                  numord, option, param)
!
! person_in_charge: nicolas.sellenet at edf.fr
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
!-----------------------------------------------------------------------
!     LECTURE FICHIER MED - LOCALISATION POINTS DE GAUSS
!     -    -            -                  -         --
!-----------------------------------------------------------------------
!     IN :
!       NROFIC : UNITE LOGIQUE DU FICHIER MED
!       LIGREL : NOM DU LIGREL
!       NOCHMD : NOM DU CHAMP MED
!       NBMA   : NOMBRE DE MAILLES DU MAILLAGE
!       NTYPEL : NOMBRE TOTAL DE TYPES DE MAILLE (=27)
!       NPGMAX : NOMBRE DE PG MAX (=27)
!       NUMPT  : NUMERO DE PAS DE TEMPS EVENTUEL
!       NUMORD : NUMERO D'ORDRE EVENTUEL DU CHAMP
!
!     OUT:
!       PGMAIL : NOMBRE DE POINTS DE GAUSS PAR MAILLE (ASTER)
!       PGMMIL : NOMBRE DE POINTS DE GAUSS PAR MAILLE (MED)
!     IN/OUT:
!       INDPG  : TABLEAU D'INDICES DETERMINANT L'ORDRE DES POINTS
!                DE GAUSS DANS UN ELEMENT DE REFERENCE :
!                INDPG(K,I_MED)=I_ASTER :
!                   - K = NUM DU TYPE DE MAILLE
!                   - I_ASTER = NUMERO LOCAL DU PG DANS L'ELEMENT
!                               DE REFERENCE ASTER
!                   - I_MED  = NUMERO LOCAL DU PG DANS L'ELEMENT
!                               DE REFERENCE MED
!
!-----------------------------------------------------------------------
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/as_mfdfin.h"
#include "asterfort/as_mfdncn.h"
#include "asterfort/as_mfdonv.h"
#include "asterfort/as_mfiope.h"
#include "asterfort/as_mlcnlc.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lrvcpg.h"
#include "asterfort/modat2.h"
#include "asterfort/typele.h"
#include "asterfort/ulisog.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nrofic, nbma, ntypel, npgmax, numpt, numord
    integer :: pgmail(nbma), pgmmil(nbma), indpg(ntypel, npgmax)
    character(len=8) :: param
    character(len=19) :: ligrel
    character(len=24) :: option
    character(len=*) :: nochmd
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRMPGA' )
!
    integer :: ntygeo
    parameter (ntygeo=19)
    integer :: edlect
    parameter (edlect=0)
    integer :: edmail
    parameter (edmail=0)
    integer :: edcomp
    parameter (edcomp=2)
!
    integer :: ifm, nivinf, ncmp, jcomp, junit
    integer :: idfimd, codret, nloc, iret, igrel, jtyelm
    integer :: j, nbgrel, jtypma, jtypge
    integer :: numte, i, ityg, ngaulu, npdt
    integer :: tygeo(ntygeo), nbpg, nbpgm
    integer :: jtmfpg, nufgpg
    integer :: dime, jtymed
    integer :: jngalu, nbtyel, nbmag, igr, ima
    integer :: jngaok
    integer :: nutyma, ipg, ipgm, jperm
    integer :: npr, n
    integer :: iopt, imod, jmod, igrd, iadgd, nec
!
    character(len=1) :: saux01
    character(len=8) :: saux08, fapg, elref, nomtm, tyele(ntygeo)
    character(len=8) :: typma(ntygeo)
    character(len=16) :: nomte, nofgpg
    character(len=24) :: liel
    character(len=64) :: nomprf, nomloc, nomam2
    character(len=200) :: nofimd
    character(len=255) :: kfic
!
    data tygeo /    1,          102,        103,        104,&
     &                203,        204,        206,        207,&
     &                208,        209,        304,        305,&
     &                306,        308,        310,        313,&
     &                315,        320,        327/
    data tyele /   'PO1',      'SE2',      'SE3',      'SE4',&
     &               'TR3',      'QU4',      'TR6',      'TR7',&
     &               'QU8',      'QU9',      'TE4',      'PY5',&
     &               'PE6',      'HE8',      'T10',      'P13',&
     &               'P15',      'H20',      'H27'/
    data typma /   'POI1    ', 'SEG2    ', 'SEG3    ', 'SEG4    ',&
     &               'TRIA3   ', 'QUAD4   ', 'TRIA6   ', 'TRIA7   ',&
     &               'QUAD8   ', 'QUAD9   ', 'TETRA4  ', 'PYRAM5  ',&
     &               'PENTA6  ', 'HEXA8   ', 'TETRA10 ', 'PYRAM13 ',&
     &               'PENTA15 ', 'HEXA20  ', 'HEXA27  '/
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
    endif
!
!  ======================================
!  == 1 : EXPLOITATION DU FICHIER MED  ==
!  ======================================
!
!  == 1.1. INITIALISATIONS
    do 11 , i=1,nbma
    pgmail(i)=0
    11 end do
!
!  == 1.2. NOM DU FICHIER MED
    call ulisog(nrofic, kfic, saux01)
    if (kfic(1:1) .eq. ' ') then
        call codent(nrofic, 'G', saux08)
        nofimd = 'fort.'//saux08
    else
        nofimd = kfic(1:200)
    endif
!
!     OUVERTURE DU FICHIER MED
    call as_mfiope(idfimd, nofimd, edlect, codret)
!
!  == 1.3. A PARTIR DU NOM DU CHAMP MED ET DE L'INDICE DU PAS DE TEMPS,
!      ON RECUPERE POUR CHAQUE TYPE DE MAILLE PRESENT:
!      - LE NOMBRE DE POINTS DE GAUSS : ZI(JNGALU)
!      - LE NOM DU TYPE GEOMETRIQUE   : ZK8(JTYMED)
!      - UN MARQUEUR DE CORRESPONDANCE : ZI(JNGAOK)
!      REMARQUE: L'INDICE DU PAS DE TEMPS EST OBTENU EN PARCOURANT
!      LA LISTE DES PAS DE TEMPS (BOUCLE 39)
    call wkvect('&&LRMPGA_TYPGEO_NBPG_MED', 'V V I', ntygeo, jngalu)
    call wkvect('&&LRMPGA_TYPGEO_TYPG_MED', 'V V K8', ntygeo, jtymed)
    call wkvect('&&LRMPGA_TYPGEO_TYEL_MED', 'V V K8', ntygeo, jtyelm)
    call wkvect('&&LRMPGA_TYPGEO_OKPG_MED', 'V V I', ntygeo, jngaok)
    call wkvect('&&LRMPGA_TYPGEO_TYPGEO', 'V V I', ntygeo, jtypge)
    nbtyel=0
    if (nivinf .gt. 1) then
        write(ifm, 2001) nochmd
    endif
!
    call as_mfdncn(idfimd, nochmd, ncmp, iret)
    call wkvect('&&LRMPGA.CNAME', 'V V K16', ncmp, jcomp)
    call wkvect('&&LRMPGA.CUNIT', 'V V K16', ncmp, junit)
    call as_mfdfin(idfimd, nochmd, nomam2, npdt, zk16(junit),&
                   zk16(jcomp), iret)
    if (npdt .gt. 0) then
!
        do 13 , ityg=1,ntygeo
        call as_mfdonv(idfimd, nochmd, edmail, tygeo(ityg), nomam2,&
                       numpt, numord, 1, nomprf, edcomp,&
                       npr, nomloc, ngaulu, n, iret)
        ASSERT(iret.eq.0)
        if (n .gt. 0) then
            nbtyel=nbtyel+1
            zk8(jtymed+nbtyel-1)=typma(ityg)
            zk8(jtyelm+nbtyel-1)=tyele(ityg)
            zi(jngalu+nbtyel-1)=ngaulu
            zi(jngaok+nbtyel-1)=0
            zi(jtypge+nbtyel-1)=tygeo(ityg)
        else
            ngaulu = 0
        endif
!
        if (nivinf .gt. 1) then
            if (ngaulu .gt. 0) then
                write (ifm,2002) tygeo(ityg),ngaulu,tyele(ityg),&
                    nomloc
            endif
        endif
!
        13     end do
    endif
    if (nivinf .gt. 1) then
        write(ifm,*) ' '
    endif
!
    call jedetr('&&LRMPGA.CNAME')
    call jedetr('&&LRMPGA.CUNIT')
!
    if (nbtyel .eq. 0) then
        call utmess('F', 'MED_77', sk=nochmd, si=nrofic)
    endif
!
!
!  == 1.4. LECTURE DU NOMBRE DE LOCALISATIONS PRESENTES DANS LE FICHIER
!     NOMBRE DE LOCALISATION(S) : NLOC
    nloc=0
    call as_mlcnlc(idfimd, nloc, iret)
    if (nivinf .gt. 1) then
        write(ifm,*) 'NOMBRE DE LOCALISATIONS DANS LE FICHIER MED =',&
        nloc
    endif
!
    call dismoi('DIM_GEOM', ligrel(1:8), 'MODELE', repi=dime)
    if (.not.(dime.eq.2.or.dime.eq.3)) then
        call utmess('F', 'MODELISA2_6')
    endif
    liel=ligrel//'.LIEL'
    call jelira(liel, 'NMAXOC', nbgrel)
!
!  =========================================
!  == 2 : EXPLOITATION DES DONNEES ASTER  ==
!  =========================================
!
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), iopt)
    call jeveuo('&CATA.TE.TYPEMA', 'L', jtypma)
!
!     ON PARCOURT LES GROUPES D'ELEMENTS PRESENTS DANS LE MODELE
    do 20 , igrel=1,nbgrel
!
!       NOM DU TYPE D'ELEMENT
    numte=typele(ligrel,igrel)
    call jenuno(jexnum('&CATA.TE.NOMTE', numte), nomte)
!
!       NOMBRE D'ELEMENTS DU GREL : NBMAG-1
    call jeveuo(jexnum(liel, igrel), 'L', igr)
    call jelira(jexnum(liel, igrel), 'LONMAX', nbmag)
!
!       MODE LOCAL ASSOCIE AU PARAMETRE PARAM DE L'OPTION OPTION
    imod = modat2(iopt, numte, param)
    if (imod .eq. 0) then
        nbpg = 0
    else
        call jeveuo(jexnum('&CATA.TE.MODELOC', imod), 'L', jmod)
!         CHAMP ELGA
        ASSERT(zi(jmod-1+1).eq.3)
!
        igrd = zi(jmod-1+2)
        call jeveuo(jexnum('&CATA.GD.DESCRIGD', igrd), 'L', iadgd)
        nec = zi(iadgd-1+3)
!         NUMERO ET NOM DE LA FAMILLE GLOBALE DE PTS GAUSS
        nufgpg = zi(jmod-1+4+nec+1)
        call jenuno(jexnum('&CATA.TM.NOFPG', nufgpg), nofgpg)
        elref= nofgpg(1:8)
        fapg = nofgpg(9:16)
!         NOMBRE DE PG : NBPG
        call jeveuo('&CATA.TM.TMFPG', 'L', jtmfpg)
        nbpg=zi(jtmfpg+nufgpg-1)
!
        nomtm=zk8(jtypma-1+numte)
!
        if (nivinf .gt. 1) then
            write(ifm,2003) nomte, fapg
        endif
!
!         ON PARCOURT LES ELEMENTS DE REFERENCE MED
        do 21 , j=1,nbtyel
!
!         SI LES ELEMENTS DE REFERENCE ASTER/MED CORRESPONDENT :
        if (zk8(jtymed+j-1) .eq. nomtm) then
!            SI LA LOCALISATION POUR LE CHAMP N'A TOUJOURS PAS ETE
!            TROUVEE DANS CELLES DU MODELE
            if (zi(jngaok+j-1) .eq. 0) then
!
!             VERIFICATION DU NOMBRE DE PG ASTER/MED
!             COMPARAISON DES COORDONNEES DES PG ASTER/MED
                call wkvect('&&LRMPGA_PERMUT', 'V V I', nbpg, jperm)
                call lrvcpg(idfimd, nbpg, zi(jngalu+j-1), nomtm, zi(jtypge+j-1),&
                            elref, zk8(jtyelm+j-1), fapg, nloc, zi(jperm),&
                            nutyma, codret)
                nbpgm = zi(jngalu+j-1)
!
!             SI LE NBRE PT GAUSS INCORRECT ET PAS DE <F>,
!             NBPG=0 : RIEN A ECRIRE DANS LRCMVE
                if (codret .eq. 4) then
                    nbpg = 0
!             SI PERMUTATIONS AU NIVEAU DES PG ASTER/MED :
                else if (codret.eq.1) then
!  ===>         REMPLISSAGE DU TABLEAU INDPG: CAS OU L'ON A
!               UNE PERMUTATION DANS LES PG MED/ASTER
                    do 220 ipgm = 1, nbpg
                        indpg(nutyma,ipgm)=zi(jperm+ipgm-1)
220                 continue
                    zi(jngaok+j-1) = 1
                else
!  ===>         SINON REMPLISSAGE DU TABLEAU INDPG: CAS OU L'ON A :
!              - ABSENCE DE LOCALISATION
!              - L UN DES PG MED N A PAS ETE IDENTIFIE A UN PG ASTER
!              - LES PG ASTER/MED CORRESPONDENT
                    do 230 ipg = 1, nbpg
                        indpg(nutyma,ipg)=ipg
230                 continue
                    zi(jngaok+j-1) = 1
                endif
!
!             DESTRUCTION DU TABLEAU TEMPORAIRE
                call jedetr('&&LRMPGA_PERMUT')
!
            endif
!
        endif
!
 21     continue
!
!      REMPLISSAGE DU TABLEAU PGMAIL : PGMAIL(NUM_MAILLE_ASTER)=NBRE_PG
        if (zi(igr) .gt. 0) then
            do 301 ima = 1, nbmag-1
                pgmail(zi(igr+ima-1))=nbpg
                pgmmil(zi(igr+ima-1))=nbpgm
301         continue
        endif
!
    endif
!
    20 end do
!
!     DESTRUCTION DES TABLEAUX TEMPORAIRES
    call jedetr('&&LRMPGA_TYPGEO_NBPG_MED')
    call jedetr('&&LRMPGA_TYPGEO_TYPG_MED')
    call jedetr('&&LRMPGA_TYPGEO_TYEL_MED')
    call jedetr('&&LRMPGA_TYPGEO_OKPG_MED')
    call jedetr('&&LRMPGA_TYPGEO_TYPGEO')
!
    call jedema()
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
    1001 format(/,10('='),a,10('='),/)
    2001 format('POUR LE CHAMP MED ', a,&
     &     /,'MAILLE ! NBRE DE PTS DE GAUSS',&
     &       ' ! ELREFE ASTER ASSOCIE ! NOM LOCALISATION',&
     &     /,72('-'))
    2002 format(i6, ' !', i10,12x,'! ', 6x, a6, 9x,'! ',  a)
    2003 format(  '  NOM DE L''ELEMENT FINI : ',a8,&
     &       /,'  FAMILLE DE PT GAUSS    : ',a8)
end subroutine
