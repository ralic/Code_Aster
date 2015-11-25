subroutine mecalr(newcal, tysd, knum, kcha, resuco,&
                  resuc1, nbordr, modele, mate, cara,&
                  nchar, ctyp)
    implicit none
! ----------------------------------------------------------------------
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
! person_in_charge: josselin.delmas at edf.fr
! ----------------------------------------------------------------------
! IN  NEWCAL : TRUE POUR UN NOUVEAU CONCEPT RESULTAT, FALSE SINON
! IN  TYSD   : TYPE DU CONCEPT ATTACHE A RESUCO
! IN  KNUM   : NOM D'OBJET DES NUMEROS D'ORDRE
! IN  KCHA   : NOM JEVEUX OU SONT STOCKEES LES CHARGES
! IN  RESUCO : NOM DE CONCEPT RESULTAT
! IN  RESUC1 : NOM DE CONCEPT DE LA COMMANDE CALC_ERREUR
! IN  CONCEP : TYPE DU CONCEPT ATTACHE A RESUC1
! IN  NBORDR : NOMBRE DE NUMEROS D'ORDRE
! IN  MODELE : NOM DU MODELE
! IN  MATE   : NOM DU CHAMP MATERIAU
! IN  CARA   : NOM DU CHAMP DES CARACTERISTIQUES ELEMENTAIRES
! IN  NCHAR  : NOMBRE DE CHARGES
! IN  CTYP   : TYPE DE CHARGE
! ----------------------------------------------------------------------
!
!     --- ARGUMENTS ---
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/calcop.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesces.h"
#include "asterfort/cetule.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlima.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/meca01.h"
#include "asterfort/mecara.h"
#include "asterfort/mecham.h"
#include "asterfort/medom1.h"
#include "asterfort/modopt.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexc1.h"
#include "asterfort/rsexc2.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsnopa.h"
#include "asterfort/singue.h"
#include "asterfort/singum.h"
#include "asterfort/sinoz1.h"
#include "asterfort/sinoz2.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbordr, nchar
    character(len=4) :: ctyp
    character(len=8) :: resuco, resuc1, modele, cara
    character(len=16) :: tysd
    character(len=19) :: knum, kcha
    character(len=24) :: mate
    aster_logical :: newcal
!
!
!     --- VARIABLES LOCALES ---
!
    character(len=6) :: nompro
    parameter(nompro='MECALR')
!
    integer :: ifm, niv
    integer :: nuord
    integer :: iordr, jordr
    integer :: iret, iret1, iret2, iret3, iret4, iret5, ireter
    integer :: nh, nbopt
    integer :: iadou, iadin
    integer :: iaux, j, ibid
    integer :: iopt
    integer :: n1, n2
    integer :: jpa, jopt, jcha
    integer :: nbac, nbpa, nbpara
    integer :: jcoor, ltymo
    integer :: nnoem, nelem, ndim, nncp
!
    character(len=4) :: type
    character(len=8) :: k8b, noma
    character(len=8) :: carele
    character(len=19) :: pfchno
    character(len=16) :: nomcmd, option, types, k16b
    character(len=19) :: leres1
    character(len=19) :: cherrs, chenes, chsins, chsinn
    character(len=24) :: cheneg, chsing, cherr1, cherr2, cherr3, cherr4
    character(len=24) :: chamgd, chsig, chsign
    character(len=24) :: chgeom, chcara(18)
    character(len=24) :: chharm, chelem
    character(len=24) :: ligrel
    character(len=24) :: nompar
    character(len=24) :: lesopt
    character(len=24) :: ligrmo
    character(len=24) :: blan24
    character(len=19) :: chvarc
!
    real(kind=8) :: prec
    real(kind=8) :: tbgrca(3)
!
    character(len=24) :: valkm(2)
    integer, pointer :: typmail(:) => null()
    integer, pointer :: dime(:) => null()
!
!
    call jemarq()
    call getres(k8b, k16b, nomcmd)
    call jerecu('V')
!               123456789012345678901234
    blan24 = '                        '
    lesopt='&&'//nompro//'.LES_OPTION'
    nh=0
    chamgd=blan24
    chgeom=blan24
    chharm=blan24
    chsig=blan24
    chelem=blan24
    chvarc='&&'//nompro//'.CHVARC'
!
    call infmaj()
    call infniv(ifm, niv)
    carele=' '
    call getvid(' ', 'CARA_ELEM', scal=carele, nbret=n1)
!
    call getvtx(' ', 'OPTION', nbval=0, nbret=n2)
    nbopt = -n2
    call wkvect(lesopt, 'V V K16', nbopt, jopt)
    call getvtx(' ', 'OPTION', nbval=nbopt, vect=zk16(jopt), nbret=n2)
    call modopt(resuco, modele, lesopt, nbopt)
    call jeveuo(lesopt, 'L', jopt)
!
!     ON RECUPERE LE TYPE DE MODE: DYNAMIQUE OU STATIQUE
    if (tysd .eq. 'MODE_MECA') then
        call rsadpa(resuco, 'L', 1, 'TYPE_MODE', 1,&
                    0, sjv=ltymo, styp=k8b)
    endif
!
    call jeveuo(knum, 'L', jordr)
    nuord=zi(jordr)
    call jeveuo(kcha//'.LCHA', 'L', jcha)
    if (newcal) then
        call rscrsd('G', resuc1, tysd, nbordr)
        call titre()
    endif
    call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrmo)
    call jenonu(jexnom(resuco//'           .NOVA', 'INST'), iret)
    call exlima(' ', 0, 'V', modele, ligrel)
!
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
!
! -- GRANDEURS CARACTERISTIQUES DE L'ETUDE
!
    call cetule(modele, tbgrca, iret)
!=======================================================================
!
!
    leres1=resuc1
!
!    ------------------------------------------------------------------
!    -- RECOPIE DES PARAMETRES DANS LA NOUVELLE SD RESULTAT
!    ------------------------------------------------------------------
!
    if (newcal) then
        nompar='&&'//nompro//'.NOMS_PARA '
        call rsnopa(resuco, 2, nompar, nbac, nbpa)
        nbpara=nbac+nbpa
        call jeveuo(nompar, 'L', jpa)
        do iaux = 1, nbordr
            iordr=zi(jordr+iaux-1)
            do j = 1, nbpara
                call rsadpa(resuco, 'L', 1, zk16(jpa+j-1), iordr,&
                            1, sjv=iadin, styp=type)
                call rsadpa(leres1, 'E', 1, zk16(jpa+j-1), iordr,&
                            1, sjv=iadou, styp=type)
                if (type(1:1) .eq. 'I') then
                    zi(iadou)=zi(iadin)
                else if (type(1:1).eq.'R') then
                    zr(iadou)=zr(iadin)
                else if (type(1:1).eq.'C') then
                    zc(iadou)=zc(iadin)
                else if (type(1:3).eq.'K80') then
                    zk80(iadou)=zk80(iadin)
                else if (type(1:3).eq.'K32') then
                    zk32(iadou)=zk32(iadin)
                else if (type(1:3).eq.'K24') then
                    zk24(iadou)=zk24(iadin)
                else if (type(1:3).eq.'K16') then
                    zk16(iadou)=zk16(iadin)
                else if (type(1:2).eq.'K8') then
                    zk8(iadou)=zk8(iadin)
                endif
            end do
        end do
    endif
!
!    ------------------------------------------------------------------
!    -- FIN RECOPIE DES PARAMETRES DANS LA NOUVELLE SD RESULTAT
!    ------------------------------------------------------------------
!
!
!
!============ DEBUT DE LA BOUCLE SUR LES OPTIONS A CALCULER ============
    do iopt = 1, nbopt
        option=zk16(jopt+iopt-1)
        if (option .eq. ' ') goto 660
!
        call jeveuo(knum, 'L', jordr)
!
!         PASSAGE CALC_CHAMP
        call calcop(option, lesopt, resuco, resuc1, knum,&
                    nbordr, ctyp, tysd, iret)
        if (iret .eq. 0) goto 660
!
        nuord=zi(jordr)
        call medom1(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco, nuord)
        call jeveuo(kcha//'.LCHA', 'L', jcha)
!
        call mecham(option, modele, cara, nh, chgeom,&
                    chcara, chharm, iret)
        if (iret .ne. 0) goto 690
!
!    ------------------------------------------------------------------
!    -- OPTIONS "SIZ1_NOEU","SIZ2_NOEU"
!    ------------------------------------------------------------------
        if (option .eq. 'SIZ1_NOEU' .or. option .eq. 'SIZ2_NOEU') then
!
!
            do iaux = 1, nbordr
                call jemarq()
                call jerecu('V')
                iordr=zi(jordr+iaux-1)
                call medom1(modele, mate, cara, kcha, nchar,&
                            ctyp, resuco, iordr)
                call jeveuo(kcha//'.LCHA', 'L', jcha)
                call mecara(cara, chcara)
                call rsexc2(1, 1, resuco, 'DEPL', iordr,&
                            chamgd, option, iret)
                if (iret .gt. 0) goto 150
                call rsexc2(1, 1, resuco, 'SIEF_ELGA', iordr,&
                            chsig, option, iret)
                if (iret .gt. 0) then
                    call utmess('A', 'CALCULEL3_7', sk=option)
                    call jedema()
                    goto 660
!
                endif
                call rsexc1(leres1, option, iordr, chsign)
                if (option .eq. 'SIZ1_NOEU') then
                    call sinoz1(modele, chsig, chsign)
                else if (option.eq.'SIZ2_NOEU') then
                    call dismoi('PROF_CHNO', chamgd, 'CHAM_NO', repk=pfchno)
                    call sinoz2(modele, pfchno, chsig, chsign)
                endif
                call rsnoch(leres1, option, iordr)
150             continue
                call jedema()
            end do
!
!    ------------------------------------------------------------------
!    -- OPTIONS DES INDICATEURS D'ERREURS
!    ------------------------------------------------------------------
            elseif (option.eq.'ERZ1_ELEM' .or.&
     &          option.eq.'ERZ2_ELEM' .or.&
     &          option.eq.'ERME_ELEM' .or. option.eq.'ERME_ELNO' .or.&
     &          option.eq.'QIRE_ELEM' .or.&
     &          option.eq.'QIRE_ELNO' .or.&
     &          option.eq.'QIZ1_ELEM' .or.&
     &          option.eq.'QIZ2_ELEM') then
!
            call meca01(option, nbordr, jordr, nchar, jcha,&
                        kcha, ctyp, tbgrca, resuco, resuc1,&
                        leres1, noma, modele, ligrmo, mate,&
                        cara, chvarc, iret)
!
            if (iret .eq. 1) then
                goto 690
!
            else if (iret.eq.2) then
                goto 660
!
            endif
!
!    ------------------------------------------------------------------
!    -- OPTION "SING_ELEM"
!    ------------------------------------------------------------------
        else if (option.eq.'SING_ELEM') then
!
            call getvr8(' ', 'PREC_ERR', scal=prec, nbret=iret1)
            if (iret1 .ne. 1) then
                call utmess('F', 'CALCULEL3_12')
            else
                if (prec .le. 0.d0) then
                    call utmess('F', 'CALCULEL3_13')
                endif
            endif
!
            types=' '
            call getvtx(' ', 'TYPE_ESTI', scal=types, nbret=ireter)
            if (ireter .gt. 0) then
                call utmess('I', 'CALCULEL3_24', sk=types)
            endif
!
! 1 - RECUPERATION DE :
!  NNOEM : NOMBRE DE NOEUDS
!  NELEM : NOMBRE D ELEMENTS FINIS (EF)
!  NDIM  : DIMENSION
!  JCOOR : ADRESSE DES COORDONNEES
!  JTYPE : ADRESSE DU TYPE D ELEMENTS FINIS
!
            call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
!
            call jeveuo(noma//'.DIME', 'L', vi=dime)
            call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
            call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
!
            nnoem=dime(1)
            nelem=dime(3)
            ndim=dime(6)
!
! 2 - CREATION D OBJETS TEMPORAIRES UTILES POUR LA SUITE
! '&&SINGUM.DIME' (DIM=3) CONTIENT
!   NBRE MAX DE NOEUDS SOMMETS CONNECTES AUX EF (NSOMMX)
!   NBRE MAX D EF CONNECTES AUX NOEUDS (NELCOM)
!   DEGRE DES EF (1 SI LINEAIRE ET 2 SI QUADRATIQUE)
! '&&SINGUM.MESU' (DIM=NELEM) CONTIENT L AIRE OU LE VOLUME DES EFS
! '&&SINGUM.CONN' (DIM=NELEM*(NSOMMX+2)) CONTIENT
!   1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N
!   2EME VALEUR = 1 SI EF EST SURFACIQUE EN 2D ET VOLUMIQUE EN 3D
!                 0 SINON
!   CONNECTIVITE EF N
! '&&SINGUM.CINV' (DIM=NNOEM*(NELCOM+2)) CONTIENT
!   1ERE VALEUR = NBRE D EF CONNECTES AU NOEUD N
!   2EME VALEUR = 0 NOEUD MILIEU OU NON CONNECTE A UN EF UTILE
!                 1 NOEUD SOMMET A L INTERIEUR + LIE A UN EF UTILE
!                 2 NOEUD SOMMET BORD + LIE A UN EF UTILE
!                 EF UTILE = EF SURF EN 2D ET VOL EN 3D
!   CONNECTIVITE INVERSE NOEUD N
!
            call singum(noma, ndim, nnoem, nelem, typmail,&
                        zr(jcoor))
!
! 3 - BOUCLE SUR LES INSTANTS DEMANDES
!
            do iaux = 1, nbordr
                call jemarq()
                iordr=zi(jordr+iaux-1)
!
                if (ireter .gt. 0) then
                    call rsexch(' ', resuco, types, iordr, cherr4,&
                                iret5)
!
                    if (iret5 .gt. 0) then
                        valkm(1)=types
                        valkm(2)=resuco
                        call utmess('A', 'CALCULEL3_26', nk=2, valk=valkm)
                        iret=1
                    endif
!
! 3.1 - RECUPERATION DE LA CARTE D ERREUR ET D ENERGIE
!       SI PLUSIEURS INDICATEURS ON PREND PAR DEFAUT
!       ERME_ELEM SI IL EST PRESENT
!       ERZ2_ELEM PAR RAPPORT A ERZ1_ELEM
!
                else
!
                    iret5=1
                    call rsexch(' ', resuco, 'ERME_ELEM', iordr, cherr1,&
                                iret1)
                    call rsexch(' ', resuco, 'ERZ1_ELEM', iordr, cherr2,&
                                iret2)
                    call rsexch(' ', resuco, 'ERZ2_ELEM', iordr, cherr3,&
                                iret3)
!
                    if (iret1 .gt. 0 .and. iret2 .gt. 0 .and. iret3 .gt. 0) then
                        call utmess('A', 'CALCULEL3_14')
                        iret=1
                    endif
!
                endif
!
                if (tysd .eq. 'EVOL_NOLI') then
                    call rsexch(' ', resuco, 'ETOT_ELEM', iordr, cheneg,&
                                iret4)
                else
                    call rsexch(' ', resuco, 'EPOT_ELEM', iordr, cheneg,&
                                iret4)
                endif
                if (iret4 .gt. 0) then
                    call utmess('A', 'CALCULEL3_29')
                endif
!
                if ((iret+iret4) .gt. 0) then
                    call utmess('A', 'CALCULEL3_36')
                    goto 250
!
                endif
! 3.2 - TRANSFORMATION DE CES DEUX CARTES EN CHAM_ELEM_S
!
                cherrs='&&'//nompro//'.ERRE'
!
                if (iret5 .eq. 0) then
                    call celces(cherr4(1:19), 'V', cherrs)
                else if (iret1.eq.0) then
                    call celces(cherr1(1:19), 'V', cherrs)
                    if ((iret2.eq.0) .or. (iret3.eq.0)) then
                        call utmess('A', 'CALCULEL3_15')
                    endif
                else if (iret3.eq.0) then
                    call celces(cherr3(1:19), 'V', cherrs)
                    if (iret2 .eq. 0) then
                        call utmess('A', 'CALCULEL3_16')
                    endif
                else if (iret2.eq.0) then
                    call celces(cherr2(1:19), 'V', cherrs)
                else
                    ASSERT(.false.)
                endif
!
                chenes='&&'//nompro//'.ENER'
                call celces(cheneg(1:19), 'V', chenes)
!
! 3.3 - ROUTINE PRINCIPALE QUI CALCULE DANS CHAQUE EF :
!       * LE DEGRE DE LA SINGULARITE
!       * LE RAPPORT ENTRE L ANCIENNE ET LA NOUVELLE TAILLE
!       DE L EF CONSIDERE
!       => CE RESULAT EST STOCKE DANS CHELEM (CHAM_ELEM)
!       CES DEUX COMPOSANTES SONT CONSTANTES PAR ELEMENT
!
                call rsexc1(leres1, option, iordr, chelem)
!
                call singue(cherrs, chenes, noma, ndim, nnoem,&
                            nelem, zr( jcoor), prec, ligrmo, chelem,&
                            types)
!
                call rsnoch(leres1, option, iordr)
!
! 3.4 - DESTRUCTION DES CHAM_ELEM_S
!
                call detrsd('CHAM_ELEM_S', cherrs)
                call detrsd('CHAM_ELEM_S', chenes)
!
250             continue
                call jedema()
            end do
!
! 4 - DESTRUCTION DES OBJETS TEMPORAIRES
!
            call jedetr('&&SINGUM.DIME           ')
            call jedetr('&&SINGUM.MESU           ')
            call jedetr('&&SINGUM.CONN           ')
            call jedetr('&&SINGUM.CINV           ')
!    ------------------------------------------------------------------
!    -- OPTION "SING_ELNO"
!    ------------------------------------------------------------------
        else if (option.eq.'SING_ELNO') then
            do iaux = 1, nbordr
                call jemarq()
                iordr=zi(jordr+iaux-1)
!
! 1 - RECUPERATION DE LA CARTE DE SINGULARITE
!
                call rsexc2(1, 1, resuco, 'SING_ELEM', iordr,&
                            chsing, option, iret1)
!
                if (iret1 .gt. 0) goto 270
!
! 2 - TRANSFORMATION DE CE CHAMP EN CHAM_ELEM_S
!
                chsins='&&'//nompro//'.SING'
                call celces(chsing(1:19), 'V', chsins)
!
! 3 - TRANSFOMATION DU CHAMP CHSINS ELEM EN ELNO
!
                chsinn='&&'//nompro//'.SINN'
                call cesces(chsins, 'ELNO', ' ', ' ', ' ',&
                            'V', chsinn)
!
! 4 - STOCKAGE
!
                call rsexc1(leres1, option, iordr, chelem)
!
                call cescel(chsinn, ligrmo(1:19), 'SING_ELNO', 'PSINGNO', 'NON',&
                            nncp, 'G', chelem(1:19), 'F', ibid)
!
                call rsnoch(leres1, option, iordr)
!
! 5 - DESTRUCTION DES CHAM_ELEM_S
!
                call detrsd('CHAM_ELEM_S', chsins)
                call detrsd('CHAM_ELEM_S', chsinn)
!
270             continue
                call jedema()
            end do
!
!      -----------------------------------------------------------------
!
        else
            call utmess('A', 'CALCULEL3_22', sk=option)
        endif
!
660     continue
    end do
!
!============= FIN DE LA BOUCLE SUR LES OPTIONS A CALCULER =============
!
690 continue
    call jedema()
end subroutine
