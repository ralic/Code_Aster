subroutine libint(imped, nume91, nbint, lisint, nbeq1)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
!
!---------------------------------------------------------------C
!--       ROUTINE XXXXX3        M. CORUS - AOUT 2011          --C
!--       CONSTRUCTION DE LA MATRICE LIBRE A L'INTERFACE      --C
!--                                                           --C
!--   REALISE APRES TEST DE LA METHODE DECRITE DANS LA DOC R3 --C
!--   (QUI MARCHE PAS) CONSSITANT A JOUER SUR LES COEFF DES   --C
!--   MULTIPLICATEURS DE LAGRANGE                             --C
!--                                                           --C
!--                                                           --C
!--       METHODE PERSO : ON VIRE LES LAGRANGES :             --C
!--          K(IND_LAG,IND_LAG)=IDENTITE*COEFF_LAGRANGE       --C
!--          K(IND_INTERF,IND_LAG)=0                          --C
!--  ET DONC K(IND_LAG,IND_INTERF)=0                          --C
!--                                                           --C
!--  AVANTAGES : ON CONSERVE LA NUMROTATION                   --C
!--              ON PEUT "MIXER" DES RESULTATS                --C
!--                  INTERFACE LIBRE / INTERFACE FIXE         --C
!--                                                           --C
!--  INCONVENIENT : ON RISQUE DE FAIRE N'IMPORTE QUOI SI      --C
!--                 ON NE FAIT PAS UN PEU ATTENTION           --C
!--                                                           --C
!-- RECOMMANDATION : N'UTILISER CETTE ROUTINE QUE SUR UNE     --C
!--                  COPIE DE LA MATICE DE RAIDEUR INITIALE   --C
!--                                                           --C
!--                                                           --C
!-- APRES TEST DE LA METHODE DECRITE DANS LA DOC R3           --C
!-- (QUI MARCHE PAS) CONSSITANT A JOUER SUR LES COEFF DES     --C
!-- MULTIPLICATEURS DE LAGRANGE                               --C
!--                                                           --C
!---------------------------------------------------------------C
!--   VARIABLES E/S  :
!--   IMPED    /IN/  : NOM K19 DE LA MATRICE DE RAIDEUR
!--   NUME91   /IN/  : NOM DU NUME_DDL ASSOCIE
!--   NBINT    /IN/  : NOMBRE D'INTERFACE DONT IL FAUT LIBERER
!--                       LES LAGRANGES
!--   LISINT   /IN/  : LISTE DES NOMS D'INTERFACES PERMETTANT DE
!--                       RECUPERER LES DDL CONCERNES PAR LA LIBERAtION
!--                       VOIR DANS LE CODE ET DANS OP0091 POUR UNE
!--                       UTILISATION DANS UN AUTRE CADRE
!--   NBEQ1    /IN/  : NB DE DDL DE LA MATRICE
!
!
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/ddllag.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    character(len=8) :: kb
    character(len=19) :: imped, nume91
    character(len=24) :: indin1
    integer :: j1, k1, l1, m1, n1, nbeq1, llint1, nbddl1, lintf, nbint, lklibr
    integer :: lnume, lag1, lag2, ind, lsmhc, ldelg
    real(kind=8) :: abs
    character(len=24) :: lisint
!
    call jeveuo(lisint, 'L', lintf)
!-- RECUPERATION DE LA MATRICE DE RAIDEUR
    call jeveuo(jexnum(imped(1:19)//'.VALM', 1), 'E', lklibr)
!
!-- RECUPERATION DES INFOS DU NUME_DDL
    call jeveuo(nume91(1:14)//'.SMOS.SMDI', 'L', lnume)
    call jeveuo(nume91(1:14)//'.SMOS.SMHC', 'L', lsmhc)
    call jeveuo(nume91(1:14)//'.NUME.DELG', 'L', ldelg)
!
!
    do 180 k1 = 1, nbint
!
        indin1='&&VEC_DDL_INTF_'//zk8(lintf+k1-1)
        call jeveuo(indin1, 'L', llint1)
        call jelira(indin1, 'LONMAX', nbddl1, kb)
!
        do 190 m1 = 1, nbddl1
            if (zi(llint1+m1-1) .gt. 0) then
                call ddllag(nume91, zi(llint1+m1-1), nbeq1, lag1, lag2)
!-- SUPRESSION DES COUPLAGES L1 / L2
                if (lag1 .gt. 1) then
                    l1=zi(lnume+lag1-1)-zi(lnume+lag1-2)-1
                    ind=zi(lnume+lag1-2)
                    do 230 n1 = 1, l1
                        zr(lklibr+ind+n1-1)=0.d0
230                  continue
                endif
                if (lag2 .gt. 1) then
                    l1=zi(lnume+lag2-1)-zi(lnume+lag2-2)-1
                    ind=zi(lnume+lag2-2)
                    do 240 n1 = 1, l1
                        zr(lklibr+ind+n1-1)=0.d0
240                  continue
                endif
!
!-- SUPPRESSION DES COUPLAGES EQ / L1
                if (zi(llint1+m1-1) .gt. 1) then
                    l1=zi(lnume+zi(llint1+m1-1)-1)- zi(lnume+zi(&
                    llint1+m1-1)-2)-1
                    ind=zi(lnume+zi(llint1+m1-1)-2)
                    do 250 j1 = 1, l1
!-- ON TESTE DANS LE NUME.DELG SI LA VALEUR EST NEGATIVE
                        if (zi(ldelg+zi4(lsmhc+ind+j1-1)-1) .lt. 0) then
                            zr(lklibr+ind+j1-1)=0.d0
                        endif
250                  continue
                endif
!-- ON REND LA DIAGONALE POSITIVE
                zr(lklibr+zi(lnume+lag1-1)-1)= abs(zr(lklibr+zi(lnume+&
                lag1-1)-1))
                zr(lklibr+zi(lnume+lag2-1)-1)= abs(zr(lklibr+zi(lnume+&
                lag2-1)-1))
            endif
!
190      continue
!
180  end do
!
end subroutine
