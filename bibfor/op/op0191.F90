subroutine op0191()
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
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
    implicit none
! ----------------------------------------------------------------------
!
!     COMMANDE : MODI_REPERE
!
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesfus.h"
#include "asterfort/chrpel.h"
#include "asterfort/chrpno.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsinfo.h"
#include "asterfort/rslesd.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsnopa.h"
#include "asterfort/rsutnu.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
!
    integer :: n0, nbordr, iret, nocc, i, j, np, iordr
    integer :: n1, nbcmp, iord, ioc, ibid, nc
    integer :: jordr, nbnosy, jpa, iadin, iadou
    integer :: nbpara, nbac, nbpa, ifm, niv, nncp
    real(kind=8) :: prec
    real(kind=8) :: lcoer(2)
    complex(kind=8) :: lcoec(2)
    character(len=8) :: crit, tych, nomma, modele
    character(len=8) :: carele, k8bid, exipla, exicoq
    character(len=16) :: concep, nomcmd, option, tysd, type, repere
    character(len=19) :: knum, resuou, kbid, resuin
    character(len=19) :: chams1, chams0, chafus, chs(2), ligrel
    character(len=24) :: nompar, champ0, champ1
    character(len=24) :: valk(2)
!
    logical :: lreuse, lcumu(2), lcoc(2)
!
    data lcumu/.false.,.false./
    data lcoc/.false.,.false./
    data lcoer/1.d0,1.d0/
! ---------------------------------------------------------------------
    call jemarq()
!
! ----- RECUPERATION DU NOM DE LA COMMANDE -----
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(resuou, concep, nomcmd)
    call getvid(' ', 'RESULTAT', scal=resuin, nbret=n0)
!
    call jelira(resuin//'.DESC', 'NOMMAX', nbnosy)
    if (nbnosy .eq. 0) goto 999
!
! LE CONCEPT EST REENTRANT SI REPERE =
!            'COQUE_INTR_UTIL' OU 'COQUE_UTIL_INTR'
!
! DANS CE CAS ON CREE UNE SD RESULTAT TEMPORAIRE POUR LES CALCULS
! ET ENSUITE ON SURCHARGE RESUIN PAR LES CHAMPS MODIFIES
! STOCKES DANS RESUOU
!
    lreuse = .false.
    if (resuin .eq. resuou) then
        lreuse = .true.
        resuou ='MODIREPE'
    endif
!
    call gettco(resuin, tysd)
!
! ----- RECUPERATION DU NOMBRE DE CHAMPS SPECIFIER -----
!
    call getfac('MODI_CHAM', nocc)
!
! ----- DEFINITION DU REPERE UTILISE -----
!
    call getvtx(' ', 'REPERE', scal=repere, nbret=i)
!
! ----- RECUPERATION DES NUMEROS D'ORDRE DE LA STRUCTURE DE
! ----- DONNEES DE TYPE RESULTAT RESU A PARTIR DES VARIABLES
! ----- D'ACCES UTILISATEUR 'NUME_ORDRE','FREQ','INST','NOEUD_CMP'
! ----- (VARIABLE D'ACCES 'TOUT_ORDRE' PAR DEFAUT)
!
    knum = '&&OP0191.NUME_ORDRE'
    call getvr8(' ', 'PRECISION', scal=prec, nbret=np)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
    call rsutnu(resuin, ' ', 1, knum, nbordr,&
                prec, crit, iret)
    if (iret .eq. 10) then
        call utmess('F', 'CALCULEL4_8', sk=resuin)
    endif
    if (iret .ne. 0) then
        call utmess('F', 'ALGORITH3_41')
    endif
    call jeveuo(knum, 'L', jordr)
!
    call rscrsd('G', resuou, tysd, nbordr)
!
    do ioc = 1, nocc
!
        call getvtx('MODI_CHAM', 'NOM_CHAM', iocc=ioc, scal=option, nbret=n0)
        call getvtx('MODI_CHAM', 'TYPE_CHAM', iocc=ioc, scal=type, nbret=n0)
        call getvtx('MODI_CHAM', 'NOM_CMP', iocc=ioc, nbval=0, nbret=n1)
        nbcmp = - n1
!
        do iord = 1, nbordr
            call jemarq()
            call jerecu('V')
            iordr = zi(jordr-1+iord)
!
            call rsexch('F', resuin, option, iordr, champ0,&
                        iret)
            call dismoi('NOM_MAILLA', champ0(1:19), 'CHAMP', repk=nomma)
            call dismoi('TYPE_CHAMP', champ0, 'CHAMP', repk=tych, arret='C',&
                        ier=iret)
!
            call rsexch(' ', resuou, option, iordr, champ1,&
                        iret)
!           CHAMP1 SERA ENSUITE RECREE SUR LA BASE GLOBALE
            call copisd('CHAMP_GD', 'V', champ0, champ1)
!
! ----- RECUPERATION DU MODELE ASSOCIE AU CHAMP
            call rslesd(resuin(1:8), iordr, modele, k8bid, carele,&
                        kbid, ibid)
            if (modele .ne. '') then
                call dismoi('EXI_PLAQUE', modele, 'MODELE', repk=exipla)
                call dismoi('EXI_COQUE', modele, 'MODELE', repk=exicoq)
                if (((exipla(1:3).eq.'OUI').or.(exicoq(1:3).eq.'OUI')) .and.&
                    ((type.eq.'TENS_2D').or.(type.eq.'TENS_3D')) .and.&
                    ((repere.eq.'CYLINDRIQUE').or.( repere.eq.'UTILISATEUR'))) then
                    call utmess('F', 'ALGORITH3_7')
                endif
            endif
!
! ----- RECUPERATION DE LA NATURE DES CHAMPS
! ----- (CHAM_NO OU CHAM_ELEM)
!
            if (tych(1:4) .eq. 'NOEU') then
                call chrpno(champ1, repere, nbcmp, ioc, type)
            else if (tych(1:2).eq.'EL') then
                call chrpel(champ1, repere, nbcmp, ioc, type,&
                            option, modele, carele, champ0)
            else
                valk(1) = tych
                valk(2) = champ1
                call utmess('A', 'ALGORITH9_69', nk=2, valk=valk)
            endif
!
            call rsnoch(resuou, option, iordr)
!
            call jedema()
        end do
    end do
!
    nompar = '&&OP0191.NOMS_PARA'
    call rsnopa(resuin, 2, nompar, nbac, nbpa)
    nbpara = nbac + nbpa
    call jeveuo(nompar, 'L', jpa)
    do iord = 1, nbordr
        iordr = zi(jordr-1+iord)
        do j = 1, nbpara
            call rsadpa(resuin, 'L', 1, zk16(jpa+j-1), iordr,&
                        1, sjv=iadin, styp=type)
            call rsadpa(resuou, 'E', 1, zk16(jpa+j-1), iordr,&
                        1, sjv=iadou, styp=type)
            if (type(1:1) .eq. 'I') then
                zi(iadou) = zi(iadin)
            else if (type(1:1).eq.'R') then
                zr(iadou) = zr(iadin)
            else if (type(1:1).eq.'C') then
                zc(iadou) = zc(iadin)
            else if (type(1:3).eq.'K80') then
                zk80(iadou) = zk80(iadin)
            else if (type(1:3).eq.'K32') then
                zk32(iadou) = zk32(iadin)
            else if (type(1:3).eq.'K24') then
                zk24(iadou) = zk24(iadin)
            else if (type(1:3).eq.'K16') then
                zk16(iadou) = zk16(iadin)
            else if (type(1:2).eq.'K8') then
                zk8(iadou) = zk8(iadin)
            endif
        end do
    end do
!
    call titre()
!
    if (niv .eq. 2) call rsinfo(resuou, ifm)
!
999 continue
!
!     -- CREATION DE L'OBJET .REFD SI NECESSAIRE:
!     -------------------------------------------
    call refdcp(resuin, resuou)
!
!
! TRAITEMENT DU CAS OU IL Y A REENTRANCE
! UTILISE SI LE MOT CLE REPERE VAUT
!    'COQUE_INTR_UTIL' OU 'COQUE_UTIL_INTR'
!
    if (lreuse) then
!
        do ioc = 1, nocc
!
            call getvtx('MODI_CHAM', 'NOM_CHAM', iocc=ioc, scal=option, nbret=n0)
!
            do iord = 1, nbordr
                call jemarq()
                call jerecu('V')
                iordr = zi(jordr-1+iord)
!
                call rsexch('F', resuin, option, iordr, champ0,&
                            iret)
                call rsexch(' ', resuou, option, iordr, champ1,&
                            iret)
!
                chams0='&&CHRPEL.CHAMS0'
                chams1='&&CHRPEL.CHAMS1'
                chafus='&&CHRPEL.CHAFUS'
                chs(1) =chams0
                chs(2) =chams1
                call celces(champ0, 'V', chams0)
                call celces(champ1, 'V', chams1)
                call cesfus(2, chs, lcumu, lcoer, lcoec,&
                            lcoc(1), 'V', chafus)
                call dismoi('NOM_LIGREL', champ0, 'CHAM_ELEM', repk=ligrel)
                call cescel(chafus, ligrel, option, ' ', 'NAN',&
                            nncp, 'G', champ0, 'F', ibid)
                call detrsd('CHAMP', champ1)
                call jedema()
!
            end do
        end do
!
        call detrsd('CHAMP', chams0)
        call detrsd('CHAMP', chams1)
        call detrsd('CHAMP', chafus)
        call detrsd('RESULTAT', resuou)
    endif
!
    call jedema()
!
end subroutine
