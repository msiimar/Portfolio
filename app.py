# Core packages
import streamlit as st
st.set_page_config(page_title="Coinbase Dashboard", layout='wide', initial_sidebar_state='auto')

import pandas as pd
import plotly.graph_objects as go
import altair as alt
import plotly.express as px
import json
import warnings
warnings.filterwarnings('ignore')

# Setting Streamlit color theme
alt.renderers.enable(embed_options={'theme': 'quartz'})

# Importing data
#~/Desktop/Stock Project/cryptoData/
data = pd.read_csv('coinData.csv')
prices = pd.read_csv('Prices.csv').rename(columns = {'Trans. Date': 'Date'})
avg_price = pd.read_csv('AvgPrice.csv')
portfolio = pd.read_csv('portfolio.csv')
crypto = pd.read_csv('cryptoTransactions.csv')
currency = pd.read_csv('currencyTransactions.csv')
risk_df = pd.read_csv('fearData.csv')
index = pd.read_csv('indexData.csv')
coinTable = pd.read_csv('coinTable.csv')

#../Stock Project/cryptoData/
with open('cryptoJSON.json', 'r') as market:
    market = json.load(market)

# Dropping and renaming columns
#~/Desktop/Stock Project/cryptoData/
data = pd.read_csv('coinData.csv').drop(columns = 'Unnamed: 0').rename(columns = {'Hist. Date': 'Date'})
data['Date'] = pd.to_datetime(data['Date'])





def main():

	# Sidebar

	# Sidebar dropdown menu
	activity = ['Portfolio', 'Strategy', 'Investors']
	choice = st.sidebar.selectbox('Select Interest', activity)



	if choice == 'Portfolio':

		# Market fear gauge chart
		config = {'displayModeBar': False}
		fig = go.Figure(go.Indicator(
			domain={'x': [0, 1], 'y': [0, 1]},
			value=risk_df['Score'][0],
			mode="gauge+number+delta",
			title={'text': risk_df['Fear Level'][0]},
			delta={'reference': risk_df['Score'][2]},
			gauge={'axis': {'range': [None, 100]},
				   'bar': {'color': "darkblue", 'thickness': 0.30},
				   'steps': [
					   {'range': [0, 30], 'color': "red"},
					   {'range': [30, 50], 'color': 'yellow'},
					   {'range': [50, 70], 'color': 'lightgreen'},
					   {'range': [70, 100], 'color': 'green'}]}))
		# Adjusting margins and size
		fig.update_layout(
			margin=dict(l=0, r=0, b=50, t=50),
			width=250,
			height=240,
		)

		st.subheader('Traditional Markets')

		st.subheader('')
		# Layout for traditional market metric cards
		c0, c1, c2, c3, c4 = st.columns(5)

		# Traditional market metrics
		with c0:
			st.metric(index.Name[0], index.Value[0],
					  delta=str(index['Point Change'][0]) + ' ' + '(' + str(index['% Change'][0]) + ' %)')

		with c1:
			st.metric(index.Name[1], index.Value[1],
					  delta=str(index['Point Change'][1]) + ' ' + '(' + str(index['% Change'][1]) + ' %)')

		with c2:
			st.metric(index.Name[2], index.Value[2],
					  delta=str(index['Point Change'][2]) + ' ' + '(' + str(index['% Change'][2]) + ' %)')

		with c3:
			st.metric(index.Name[3], index.Value[3],
					  delta=str(index['Point Change'][3]) + ' ' + '(' + str(index['% Change'][3]) + ' %)')

		# Traditional market resources
		with c4:
			st.write('Markets:')
			st.write(' * [Wall Street Journal](https://www.wsj.com/)')
			st.write(
				' * [Yahoo Finance](https://finance.yahoo.com/?guccounter=1&guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAANCpEZowy6FIk2H6-yaZ_ThU5V4RplEogh9yLY_sGx6G9QEcAfjBN1w-lakmH8vLwFY2O7M9QaxbZYHT_5GBAiFkFcH-snBwzzdm-RiXSCg_Nm2JxQovImd79EjAchv_c0xtvwZEKVGmfl1WIGo2YtX8Rlcirg0uCY0n1vsvybfC)')


		# Functional that color codes a dataframe
		def color(val):
			if val > 0:
				color = 'green'
			else:
				color = 'red'

			return 'color: %s' % color

		# Adding horizontal line between sections
		line = '<hr>'
		st.markdown(line, unsafe_allow_html=True)
		# Crypto market metrics section
		st.subheader('Cryptocurrency Markets')

		st.subheader('')

		# Crypto market metrics
		market_cap = market.get('Market')[0]
		change = float(market.get('Market')[1])
		trade_vol = market.get('Market')[2]
		bit_dom = market.get('Market')[3] + '%'
		nr_coins = int(market.get('Market')[4])

		# Layout for crypto market metric cards
		c1, c2, c3, c4 = st.columns(4)

		# Crypto market metrics
		with c1:
			st.metric('Market Capitalization', market_cap, delta=str(change) + ' %')

		with c2:
			st.metric('24h Trading Volume', trade_vol)

		with c3:
			st.metric('Bitcoin Market Cap Dominance', bit_dom)

		with c4:
			st.metric('# of Coins', nr_coins)

		# Layout for crypto market table and graphs
		c1, c2, c3 = st.columns([2, 1.25, 0.75])

		with c1:
			st.text('')

			# CSS code that hides dataframe index column
			hide_table_row_index = """
											 <style>
												tbody th {display:none}
												.blank {display:none}
												</style>
												"""

			st.markdown(hide_table_row_index, unsafe_allow_html=True)

			# Bitcoin and Ethereum table
			st.markdown(coinTable.iloc[:, 1:].style.applymap(color, subset=['1h', '24h', '7d']).format(
				precision=2).set_table_styles(
				[{'selector': 'th', 'props': [('font-size', '15pt'), ('text-align', 'center')]}]).set_properties(
				**{'font-size': '14pt'}).render(), unsafe_allow_html=True)

		# Fear index historic data
		with c2:
			st.plotly_chart(fig, config=config, use_container_width=True)


		# Fear index chart
		with c3:
			st.subheader('')
			st.subheader('')
			st.info(
				'Historical Fear Levels  \n  Last Week: {} ({})  \n Last Month: {} ({})  \n'.format(
					risk_df.Score[2], risk_df['Fear Level'][2], risk_df.Score[3], risk_df['Fear Level'][3]))


		# Adding horizontal lines between sections
		line = '<hr>'
		st.markdown(line, unsafe_allow_html=True)

		st.subheader('Crypto Portfolio Overview')

		st.subheader('')

		# Portfolio metrics
		total_inv = int(crypto['Value At Transaction'].sum()) # Invested capital
		val_today = int(portfolio['Value Today'].sum())	# Portfolio value
		pl = int(val_today - total_inv) # Profit/loss
		roi = int(((val_today - total_inv) / total_inv) * 100) # ROI
		cash = int(currency['Deposit Amount'].sum()) # Available cash


		co0,co1,co2,co3, co4, co5 = st.columns(6) # Home page metrics row layout

		icon = '<p style="font-family:Courier; text-align:center; color:Orange; font-size: 30px;">AirShip Capital</p>'

		with co0:
			st.markdown(icon, unsafe_allow_html = True)

		# Crypto portfolio metric cards
		with co1:
			st.metric('Total Invested Capital', str(total_inv) + ' €')

		with co2:
			st.metric('Portfolio Value', str(val_today) + ' €')
		with co3:
			st.metric('Profit/Loss', str(pl) + ' €', delta=str(roi) + ' %')

		with co4:
			st.metric('Free Cash', str(cash) + ' €')

		with co5:
			radio = st.radio('Select Chart Type', ['Tree', 'Bar'])



		# Portfolio P&L time series graph
		port_pl = alt.Chart(pd.DataFrame(data.groupby('Date')
										['Net P&L'].sum()).reset_index())\
										.mark_line().encode(x = 'Date',
															y = alt.Y('Net P&L', title = 'Net P&L'),
															 tooltip = [alt.Tooltip('Net P&L'),
																		alt.Tooltip('Date')])

		# Adding zero line to the graph
		vert_line =alt.Chart(pd.DataFrame({'y': [1]})).mark_rule(strokeDash=[12, 6], size=0.3, color = 'white').encode(y=alt.Y('y'))
		# Complete graph
		portfolio_pl = (port_pl + vert_line).configure_axis(grid=False).configure_view(strokeWidth=0).interactive()


		# Portfolio asset allocation bar chart
		portfolio_bar = alt.Chart(portfolio).mark_bar().encode(
			x=alt.X('Currency', sort='-y'),
			y=alt.Y('Value Today'),
			color = alt.Color('Currency', legend = None, scale=alt.Scale(scheme='tableau10')),
			tooltip=[alt.Tooltip('Currency'),
					 alt.Tooltip('Value Today')]).configure_axis(grid=False).configure_view(strokeWidth=0)

		# Portfolio asset allocation tree map
		config = {'displayModeBar': False}
		tree = px.treemap(portfolio, path=['Currency'], values='Value Today', width=700, height=280)
		tree.data[0].hovertemplate = 'Currency: %{label}<br> Value Today: %{value}'
		tree.update_layout(
			margin=dict(l=0, r=0, b=0, t=0)
		)
		tree.update_traces(hovertemplate = None)


		# Layout for the portfolio graphs
		c1, c2 = st.columns(2)

		with c1:
			# Adding asset allocation time series char to the dashboard
			st.altair_chart(portfolio_pl, use_container_width=True)

		with c2:
			# Radio buttons that enable to choose between bar chart and tree map
			if radio == 'Bar':
				st.altair_chart(portfolio_bar, use_container_width=True) #

			else:
				st.plotly_chart(tree, use_container_width=True, config = config)


		# Layout for specific asset graphs
		col1, col2, col3, col4 = st.columns([2,1,1,4])

		with col1:

			# Dropdown menu that enables to choose a specific asset from the portfolio
			coin = st.selectbox('Choose Crypto Currency', list(data.Label.unique()))
			# Radio button that enables to choose a metric for time series graph
			metric = st.radio('Choose Metric', ['Close', 'Net P&L', 'Volume'])

			# Expander that shows transaction history related to the chosen crypto asset
			with st.expander('Transaction Prices'):

				# CSS code that hides dataframe index column
				hide_table_row_index = """
							            <style>
							            tbody th {display:none}
							            .blank {display:none}
							            </style>
							            """

				st.markdown(hide_table_row_index, unsafe_allow_html=True)

				# Weighted average price in an info box
				avgPrice = round(avg_price.loc[avg_price['Crypto Currency'] == coin]['Weighted Avg Price'].values[0], 2)
				st.info('Weighted Average Price {}'.format(avgPrice))
				# Transactions table
				st.table(prices.loc[prices['Crypto Currency'] == coin][
							 ['Date', 'Coin Amount', 'Price At Transaction', 'Value At Transaction']])


		with col2:

			# Metrics for metric cards on the individual assets row
			coin_data = data.loc[data['Label'] == coin]
			recent = coin_data.tail(1)
			pl = round(recent['Net P&L'].values[0], 2) # P&L
			iv = round(recent['Investment Value'].values[0], 2) # Investment Value
			close = round(recent['Close'].values[0], 2) # Close Price
			va = round(recent['Value At Transaction'].values[0], 2) # Value at transaction
			amount = round(recent['Coin Amount'].values[0], 2) # Coin amount
			roi = round((iv - va) / va * 100, 2) # RoI
			avgPrice = round(avg_price.loc[avg_price['Crypto Currency'] == coin]['Weighted Avg Price'].values[0], 2) # Weighted average price

			# Adding metric cards to the dashboard
			st.metric('Invested Capital', str(va)  + ' €', delta=str(pl))
			st.metric('Coin Amount', amount)
			st.metric('Close Price', str(close) + ' €')


		with col3:

			# Adding metric cards to the dashboard
			st.metric('Investment Value', str(iv)  + ' €', delta=str(roi) + '%')
			st.metric('Average Price', str(avgPrice)  + ' €')

		with col4:

			# Time series graph on the second row on the home page
			if metric != 'Close':
				# Time series graph
				c = alt.Chart(coin_data).mark_line().encode(x='Date', y=alt.Y(metric, title = metric),
														tooltip = [alt.Tooltip('Date'),
																	alt.Tooltip(metric)])

				# Adding zero line
				vert_line = alt.Chart(pd.DataFrame({'y': [1]})).mark_rule(strokeDash=[12, 6], size=0.3, color='white').encode(y='y')
				performance = (c + vert_line).configure_axis(grid=False).configure_view(strokeWidth=0).interactive()

				st.altair_chart(performance, use_container_width=True)

			else:
				# When metric is Close price
				# Time series graph
				line = alt.Chart(coin_data).mark_line().encode(x='Date', y='Close', tooltip = [alt.Tooltip('Date'), alt.Tooltip('Close')])
				# Adding dots the line chart on dates when investments were made
				mark = alt.Chart(coin_data.loc[(coin_data['Crypto Currency'] != '-')]).mark_circle(color='red', size=100).encode(x='Date', y=alt.Y('Close', title = 'Close'),
									  tooltip=[alt.Tooltip('Date'),
											   alt.Tooltip('Close')])

				final = (line + mark).configure_axis(grid=False).configure_view(strokeWidth=0).interactive()
				st.altair_chart(final, use_container_width=True)


if __name__ == '__main__':
	main()
